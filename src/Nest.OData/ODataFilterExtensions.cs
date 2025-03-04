#if USE_ODATA_V7
using Microsoft.AspNet.OData.Query;
using Microsoft.OData;
#else
using Microsoft.AspNetCore.OData.Query;
using Microsoft.OData;
#endif
using Microsoft.OData.Edm;
using Microsoft.OData.UriParser;

#nullable disable
namespace Nest.OData
{
    /// <summary>
    /// https://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html
    /// </summary>
    public static class ODataFilterExtensions
    {
        public static SearchDescriptor<T> Filter<T>(this SearchDescriptor<T> searchDescriptor, FilterQueryOption filter) where T : class
        {
            if (filter?.FilterClause?.Expression == null)
            {
                return searchDescriptor;
            }

            var queryContainer = filter.ToQueryContainer(new ODataExpressionContext
            {
                Type = typeof(T),
            });

            return searchDescriptor.Query(q => queryContainer);
        }

        public static QueryContainer ToQueryContainer(this FilterQueryOption filter, ODataExpressionContext context = null)
        {
            if (filter?.FilterClause?.Expression == null)
            {
                return new MatchAllQuery();
            }

            var queryContainer = TranslateExpression(filter.FilterClause.Expression, context);

            if (queryContainer == null)
            {
                return new MatchAllQuery();
            }

            return queryContainer;
        }

        internal static QueryContainer TranslateExpression(QueryNode node, ODataExpressionContext context = null)
        {
            if (ShouldOptimizeOrOperation(node))
            {
                return OptimizeIdenticalFunctionCalls(node as BinaryOperatorNode, context);
            }

            return node.Kind switch
            {
                QueryNodeKind.Any => TranslateAnyNode(node as AnyNode, context),
                QueryNodeKind.All => TranslateAllNode(node as AllNode, context),
                QueryNodeKind.In => TranslateInNode(node as InNode, context),
                QueryNodeKind.BinaryOperator => TranslateOperatorNode(node as BinaryOperatorNode, context),
                QueryNodeKind.SingleValueFunctionCall => TranslateFunctionCallNode(node as SingleValueFunctionCallNode, context),
                QueryNodeKind.Convert => TranslateExpression(((ConvertNode)node).Source, context),
                QueryNodeKind.UnaryOperator => TranslateUnaryOperatorNode(node as UnaryOperatorNode, context),
                QueryNodeKind.SingleValuePropertyAccess => null,
                QueryNodeKind.Constant => null,
                _ => throw new NotImplementedException($"Unsupported node type: {node.Kind}"),
            };
        }

        private static bool ShouldOptimizeOrOperation(QueryNode node)
        {
            return node is BinaryOperatorNode binaryNode &&
                   binaryNode.OperatorKind == BinaryOperatorKind.Or &&
                   binaryNode.Left is SingleValueFunctionCallNode &&
                   binaryNode.Right is SingleValueFunctionCallNode;
        }

        private static QueryContainer OptimizeIdenticalFunctionCalls(BinaryOperatorNode node, ODataExpressionContext context)
        {
            var leftFunc = node.Left as SingleValueFunctionCallNode;
            var rightFunc = node.Right as SingleValueFunctionCallNode;

            var functionCalls = new[]
            {
                (Node: leftFunc, IsNested: IsNestedFunctionCall(leftFunc)),
                (Node: rightFunc, IsNested: IsNestedFunctionCall(rightFunc))
            };

            if (AreFunctionCallsIdentical(functionCalls[0], functionCalls[1]))
            {
                return TranslateExpression(node.Left, context);
            }

            return TranslateExpression(node.Right, context);
        }

        private static bool AreFunctionCallsIdentical((SingleValueFunctionCallNode Node, bool IsNested) first, (SingleValueFunctionCallNode Node, bool IsNested) other)
        {
            if (HasIdenticalFunctionSignature(first.Node, other.Node) == false)
            {
                return false;
            }

            if (HasIdenticalParameters(first.Node, other.Node) == false)
            {
                return false;
            }

            if (first.IsNested != other.IsNested)
            {
                return false;
            }

            return true;
        }

        private static bool HasIdenticalFunctionSignature(SingleValueFunctionCallNode first, SingleValueFunctionCallNode other)
        {
            return string.Equals(first.Name, other.Name, StringComparison.OrdinalIgnoreCase) && first.Parameters.Count() == other.Parameters.Count();
        }

        private static bool HasIdenticalParameters(SingleValueFunctionCallNode first, SingleValueFunctionCallNode other)
        {
            if (first.Parameters.First().ToString() != other.Parameters.First().ToString())
            {
                return false;
            }

            var firstValue = ExtractValue(first.Parameters.Last())?.ToString();
            var otherValue = ExtractValue(other.Parameters.Last())?.ToString();

            return string.Equals(firstValue, otherValue);
        }

        private static QueryContainer TranslateAnyNode(AnyNode node, ODataExpressionContext context = null)
        {
            var fullyQualifiedFieldName = ODataHelpers.ExtractFullyQualifiedFieldName(node.Source, context);

            var isNavigationProperty = node.Source is CollectionNavigationNode ||
                ((node.Source is CollectionPropertyAccessNode collectionNode) && ODataHelpers.IsNavigationNode(collectionNode.Source.Kind));

            var query = TranslateExpression(node.Body, new ODataExpressionContext
            {
                PathPrefix = fullyQualifiedFieldName,
                Type = context.Type,
            });

            if (isNavigationProperty)
            {
                return new NestedQuery
                {
                    Path = ODataHelpers.ExtractNestedPath(fullyQualifiedFieldName),
                    Query = query,
                };
            }

            return query;
        }

        private static QueryContainer TranslateAllNode(AllNode node, ODataExpressionContext context = null)
        {
            var fullyQualifiedFieldName = ODataHelpers.ExtractFullyQualifiedFieldName(node.Source, context);

            var isNavigationProperty = node.Source is CollectionNavigationNode ||
                ((node.Source is CollectionPropertyAccessNode collectionNode) && ODataHelpers.IsNavigationNode(collectionNode.Source.Kind));

            var query = new BoolQuery
            {
                MustNot =
                [
                    !TranslateExpression(node.Body, new ODataExpressionContext
                    {
                        PathPrefix = fullyQualifiedFieldName,
                        Type = context.Type,
                    })
                ]
            };

            if (isNavigationProperty)
            {
                return new NestedQuery
                {
                    Path = ODataHelpers.ExtractNestedPath(fullyQualifiedFieldName),
                    Query = query
                };
            }

            return query;
        }

        private static QueryContainer TranslateInNode(InNode node, ODataExpressionContext context = null)
        {
            var fullyQualifiedFieldName = ODataHelpers.ExtractFullyQualifiedFieldName(node.Left, context);

            if (node.Right is not CollectionConstantNode collectionNode)
            {
                throw new NotImplementedException("Right node is not CollectionConstantNode!");
            }

            var values = new List<object>();

            foreach (var item in collectionNode.Collection)
            {
                var value = ExtractValue(item);

                values.Add(value);
            }

            var query = new TermsQuery { Field = fullyQualifiedFieldName, Terms = values };

            if (ExtractSourceNode(node.Left) is SingleValuePropertyAccessNode singleValueNode && ODataHelpers.IsNavigationNode(singleValueNode.Source.Kind))
            {
                return new NestedQuery
                {
                    Path = ODataHelpers.ExtractNestedPath(fullyQualifiedFieldName),
                    Query = query,
                };
            }

            return query;
        }

        private static QueryContainer TranslateOperatorNode(BinaryOperatorNode node, ODataExpressionContext context = null)
        {
            var fullyQualifiedFieldName = ODataHelpers.ExtractFullyQualifiedFieldName(node.Left, context);

            var query = node.OperatorKind switch
            {
                BinaryOperatorKind.And => TranslateAndOperations(node, context),
                BinaryOperatorKind.Or => TranslateOrOperations(node, context),
                BinaryOperatorKind.Equal => TranslateEqualOperation(node.Right, fullyQualifiedFieldName),
                BinaryOperatorKind.NotEqual => TranslateNotEqualOperation(node.Right, fullyQualifiedFieldName),
                BinaryOperatorKind.GreaterThan => new TermRangeQuery { Field = fullyQualifiedFieldName, GreaterThan = ExtractStringValue(node.Right) },
                BinaryOperatorKind.GreaterThanOrEqual => new TermRangeQuery { Field = fullyQualifiedFieldName, GreaterThanOrEqualTo = ExtractStringValue(node.Right) },
                BinaryOperatorKind.LessThan => new TermRangeQuery { Field = fullyQualifiedFieldName, LessThan = ExtractStringValue(node.Right) },
                BinaryOperatorKind.LessThanOrEqual => new TermRangeQuery { Field = fullyQualifiedFieldName, LessThanOrEqualTo = ExtractStringValue(node.Right) },
                _ => throw new NotImplementedException($"Unsupported binary operator: {node.OperatorKind}"),
            };

            if (ExtractSourceNode(node.Left) is SingleValuePropertyAccessNode singleValueNode && ODataHelpers.IsNavigationNode(singleValueNode.Source.Kind))
            {
                return new NestedQuery
                {
                    Path = ODataHelpers.ExtractNestedPath(fullyQualifiedFieldName),
                    Query = query,
                };
            }

            return query;
        }

        private static QueryContainer TranslateFunctionCallNode(SingleValueFunctionCallNode node, ODataExpressionContext context = null)
        {
            var left = node.Parameters.First();
            var right = node.Parameters.Last();
            var fullyQualifiedFieldName = ODataHelpers.ExtractFullyQualifiedFieldName(left, context);
            var value = ExtractValue(right);

            var query = node.Name.ToLower() switch
            {
                "startswith" => (QueryContainer)new PrefixQuery { Field = fullyQualifiedFieldName, Value = value },
                "endswith" => new WildcardQuery { Field = fullyQualifiedFieldName, Value = $"*{value}" },
                "contains" => new WildcardQuery { Field = fullyQualifiedFieldName, Value = $"*{value}*" },
                "substringof" => new MatchQuery { Field = fullyQualifiedFieldName, Query = value.ToString() },
                _ => throw new NotImplementedException($"Unsupported function: {node.Name}"),
            };

            if (ExtractSourceNode(left) is SingleValuePropertyAccessNode singleValueNode && ODataHelpers.IsNavigationNode(singleValueNode.Source.Kind))
            {
                return new NestedQuery
                {
                    Path = ODataHelpers.ExtractNestedPath(fullyQualifiedFieldName),
                    Query = query,
                };
            }

            return query;
        }

        private static QueryContainer TranslateOrOperations(BinaryOperatorNode node, ODataExpressionContext context = null)
        {
            var queries = new List<QueryContainer>();
            var functionCalls = new List<(SingleValueFunctionCallNode Node, bool IsNested)>();

            void Collect(QueryNode queryNode)
            {
                if (queryNode is BinaryOperatorNode binaryNode && binaryNode.OperatorKind == BinaryOperatorKind.Or)
                {
                    Collect(binaryNode.Left);
                    Collect(binaryNode.Right);
                }
                else
                {
                    if (queryNode is SingleValueFunctionCallNode funcNode)
                    {
                        var isNested = IsNestedFunctionCall(funcNode);
                        functionCalls.Add((funcNode, isNested));
                    }

                    var query = TranslateExpression(queryNode, context);
                    if (query != null)
                    {
                        queries.Add(query);
                    }
                }
            }

            Collect(node);

            if (queries.Any() == false)
            {
                return null;
            }

            return OptimizeOrQueries(queries, functionCalls);
        }

        private static bool IsNestedFunctionCall(SingleValueFunctionCallNode funcNode)
        {
            return funcNode.Parameters.First() is SingleValuePropertyAccessNode propNode && ODataHelpers.IsNavigationNode(propNode.Source.Kind);
        }

        private static QueryContainer OptimizeOrQueries(List<QueryContainer> queries, List<(SingleValueFunctionCallNode Node, bool IsNested)> functionCalls)
        {
            if (CanOptimizeFunctionCalls(functionCalls, queries.Count) == false)
            {
                return new BoolQuery { Should = queries, MinimumShouldMatch = 1 };
            }

            var first = functionCalls[0];

            return AreAllFunctionCallsIdentical(functionCalls, first) ? queries[0] : new BoolQuery { Should = queries, MinimumShouldMatch = 1 };
        }

        private static bool CanOptimizeFunctionCalls(List<(SingleValueFunctionCallNode Node, bool IsNested)> functionCalls, int queryCount)
        {
            return functionCalls.Count > 0 && functionCalls.Count == queryCount;
        }

        private static bool AreAllFunctionCallsIdentical(List<(SingleValueFunctionCallNode Node, bool IsNested)> functionCalls,
                                                       (SingleValueFunctionCallNode Node, bool IsNested) first)
        {
            return functionCalls.All(f => AreFunctionCallsIdentical(first, f));
        }

        private static QueryContainer TranslateAndOperations(BinaryOperatorNode node, ODataExpressionContext context = null)
        {
            var queries = new List<QueryContainer>();

            void Collect(QueryNode queryNode)
            {
                if (queryNode is BinaryOperatorNode binaryNode && binaryNode.OperatorKind == BinaryOperatorKind.And)
                {
                    Collect(binaryNode.Left);
                    Collect(binaryNode.Right);
                }
                else
                {
                    queries.Add(TranslateExpression(queryNode, context));
                }
            }

            Collect(node);

            if (queries.Any() == false)
            {
                return null;
            }

            // If we have a single query from an AND operation to maintain the expected structure.
            if (queries.Count == 1)
            {
                return new BoolQuery { Must = queries };
            }

            return new BoolQuery { Must = queries };
        }

        private static QueryContainer TranslateEqualOperation(SingleValueNode node, string fieldName)
        {
            var value = ExtractValue(node);

            if (value == null)
            {
                return !new ExistsQuery { Field = fieldName };
            }

            return new TermQuery { Field = fieldName, Value = value };
        }

        private static QueryContainer TranslateNotEqualOperation(SingleValueNode node, string fieldName)
        {
            var value = ExtractValue(node);

            if (value == null)
            {
                return new ExistsQuery { Field = fieldName };
            }

            return !new TermQuery { Field = fieldName, Value = value };
        }

        private static QueryNode ExtractSourceNode(QueryNode node)
        {
            if (node is ConvertNode convertNode)
            {
                return convertNode.Source;
            }

            return node;
        }

        private static object ExtractValue(QueryNode node)
        {
            if (node is ConstantNode constantNode)
            {
                var typeKind = constantNode.TypeReference?.Definition?.TypeKind;

                if (typeKind is EdmTypeKind.Enum)
                {
                    return (constantNode.Value as ODataEnumValue).Value?.ToString();
                }

                if (typeKind is EdmTypeKind.Primitive && constantNode.TypeReference.PrimitiveKind() is EdmPrimitiveTypeKind.String)
                {
                    return (constantNode.Value as string)?.ToLower();
                }

                return constantNode.Value;
            }
            else if (node is ConvertNode convertNode)
            {
                return ExtractValue(convertNode.Source);
            }

            throw new NotImplementedException("Complex values are not supported yet.");
        }

        private static string ExtractStringValue(QueryNode node)
        {
            if (node is ConstantNode constantNode)
            {
                if (constantNode.Value is DateTime dateTime)
                {
                    return dateTime.ToString("o");
                }
                else if (constantNode.Value is DateTimeOffset dateTimeOffset)
                {
                    return dateTimeOffset.ToString("o");
                }
                else
                {
                    return constantNode.Value?.ToString();
                }
            }
            else if (node is ConvertNode convertNode)
            {
                return ExtractStringValue(convertNode.Source);
            }

            throw new NotImplementedException("Complex values are not supported yet.");
        }


        private static QueryContainer TranslateUnaryOperatorNode(UnaryOperatorNode node, ODataExpressionContext context)
        {
            if (node.OperatorKind != UnaryOperatorKind.Not)
            {
                throw new NotImplementedException($"Unsupported unary operator: {node.OperatorKind}");
            }

            var operandQuery = TranslateExpression(node.Operand, context);

            if (operandQuery == null)
            {
                return null;
            }

            return new BoolQuery
            {
                MustNot = new[] { operandQuery }
            };
        }
    }
}
