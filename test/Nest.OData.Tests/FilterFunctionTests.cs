﻿using Nest.OData.Tests.Common;
using Newtonsoft.Json.Linq;
using Xunit;

namespace Nest.OData.Tests
{
    public class FilterFunctionTests
    {
        [Fact]
        public void StartsWithFunction()
        {
            var queryOptions = "$filter=startswith(Category, 'Goods')".GetODataQueryOptions<Product>();

            var elasticQuery = queryOptions.ToElasticQuery();

            Assert.NotNull(elasticQuery);

            var queryJson = elasticQuery.ToJson();

            var expectedJson = @"
            {
                ""query"": {
                    ""prefix"": {
                        ""Category"": {
                            ""value"": ""goods""
                        }
                    }
                }
            }";

            var actualJObject = JObject.Parse(queryJson);
            var expectedJObject = JObject.Parse(expectedJson);

            Assert.True(JToken.DeepEquals(expectedJObject, actualJObject), "Expected and actual JSON do not match.");
        }

        [Fact]
        public void EndsWithFunction()
        {
            var queryOptions = "$filter=endswith(Category, 'Goods')".GetODataQueryOptions<Product>();

            var elasticQuery = queryOptions.ToElasticQuery();

            Assert.NotNull(elasticQuery);

            var queryJson = elasticQuery.ToJson();

            var expectedJson = @"
            {
                ""query"": {
                    ""wildcard"": {
                        ""Category"": {
                            ""value"": ""*goods""
                        }
                    }
                }
            }";

            var actualJObject = JObject.Parse(queryJson);
            var expectedJObject = JObject.Parse(expectedJson);

            Assert.True(JToken.DeepEquals(expectedJObject, actualJObject), "Expected and actual JSON do not match.");
        }

        [Fact]
        public void ContainsFunction()
        {
            var queryOptions = "$filter=contains(Category, 'Goods')".GetODataQueryOptions<Product>();

            var elasticQuery = queryOptions.ToElasticQuery();

            Assert.NotNull(elasticQuery);

            var queryJson = elasticQuery.ToJson();

            var expectedJson = @"
            {
              ""query"": {
                ""wildcard"": {
                  ""Category"": {
                    ""value"": ""*goods*""
                  }
                }
              }
            }";

            var actualJObject = JObject.Parse(queryJson);
            var expectedJObject = JObject.Parse(expectedJson);

            Assert.True(JToken.DeepEquals(expectedJObject, actualJObject), "Expected and actual JSON do not match.");
        }

        [Fact]
        public void ContainsToLowerFunction()
        {
            var queryOptions = "$filter=contains(tolower(Category), 'Goods')".GetODataQueryOptions<Product>();

            var elasticQuery = queryOptions.ToElasticQuery();

            Assert.NotNull(elasticQuery);

            var queryJson = elasticQuery.ToJson();

            var expectedJson = @"
            {
              ""query"": {
                ""wildcard"": {
                  ""Category"": {
                    ""value"": ""*goods*""
                  }
                }
              }
            }";

            var actualJObject = JObject.Parse(queryJson);
            var expectedJObject = JObject.Parse(expectedJson);

            Assert.True(JToken.DeepEquals(expectedJObject, actualJObject), "Expected and actual JSON do not match.");
        }

        [Fact]
        public void MultipleIdenticalFunctionCalls()
        {
            var queryOptions = "$filter=(contains(Category,'Goods')) or (contains(Category,'Goods')) or (contains(Category,'Goods'))".GetODataQueryOptions<Product>();

            var elasticQuery = queryOptions.ToElasticQuery();

            Assert.NotNull(elasticQuery);

            var queryJson = elasticQuery.ToJson();

            var expectedJson = @"
            {
              ""query"": {
                ""wildcard"": {
                  ""Category"": {
                    ""value"": ""*goods*""
                  }
                }
              }
            }";

            var actualJObject = JObject.Parse(queryJson);
            var expectedJObject = JObject.Parse(expectedJson);

            Assert.True(JToken.DeepEquals(expectedJObject, actualJObject), "Expected and actual JSON do not match.");
        }

        [Fact]
        public void MultipleDifferentFunctionCalls()
        {
            var queryOptions = "$filter=(contains(Category,'Goods')) or (contains(Category,'Food')) or (contains(Name,'Merchandise'))".GetODataQueryOptions<Product>();

            var elasticQuery = queryOptions.ToElasticQuery();

            Assert.NotNull(elasticQuery);

            var queryJson = elasticQuery.ToJson();

            var expectedJson = @"
            {
              ""query"": {
                ""bool"": {
                  ""minimum_should_match"": 1,
                  ""should"": [
                    {
                      ""wildcard"": {
                        ""Category"": {
                          ""value"": ""*goods*""
                        }
                      }
                    },
                    {
                      ""wildcard"": {
                        ""Category"": {
                          ""value"": ""*food*""
                        }
                      }
                    },
                    {
                      ""wildcard"": {
                        ""Name"": {
                          ""value"": ""*merchandise*""
                        }
                      }
                    }
                  ]
                }
              }
            }";

            var actualJObject = JObject.Parse(queryJson);
            var expectedJObject = JObject.Parse(expectedJson);

            Assert.True(JToken.DeepEquals(expectedJObject, actualJObject), "Expected and actual JSON do not match.");
        }

        [Fact]
        public void MultipleOperatorsFunctionCall()
        {
            var queryOptions = "$filter=(contains(Category,'Food') and contains(Category,'Goods')) or contains(Name,'Merchandise')".GetODataQueryOptions<Product>();

            var elasticQuery = queryOptions.ToElasticQuery();

            Assert.NotNull(elasticQuery);

            var queryJson = elasticQuery.ToJson();

            var expectedJson = @"
            {
              ""query"": {
                ""bool"": {
                  ""minimum_should_match"": 1,
                  ""should"": [
                    {
                      ""bool"": {
                        ""must"": [
                          {
                            ""wildcard"": {
                              ""Category"": {
                                ""value"": ""*food*""
                              }
                            }
                          },
                          {
                            ""wildcard"": {
                              ""Category"": {
                                ""value"": ""*goods*""
                              }
                            }
                          }
                        ]
                      }
                    },
                    {
                      ""wildcard"": {
                        ""Name"": {
                          ""value"": ""*merchandise*""
                        }
                      }
                    }
                  ],
                }
              }
            }";

            var actualJObject = JObject.Parse(queryJson);
            var expectedJObject = JObject.Parse(expectedJson);

            Assert.True(JToken.DeepEquals(expectedJObject, actualJObject), "Expected and actual JSON do not match.");
        }

        [Fact]
        public void NotFunction()
        {
            var queryOptions = "$filter=not(Category eq 'Goods')".GetODataQueryOptions<Product>();

            var elasticQuery = queryOptions.ToElasticQuery();

            Assert.NotNull(elasticQuery);

            var queryJson = elasticQuery.ToJson();

            var expectedJson = @"
            {
              ""query"": {
                ""bool"": {
                  ""must_not"": [
                    {
                      ""term"": {
                        ""Category"": {
                          ""value"": ""goods""
                        }
                      }
                    }
                  ]
                }
              }
            }";

            var actualJObject = JObject.Parse(queryJson);
            var expectedJObject = JObject.Parse(expectedJson);

            Assert.True(JToken.DeepEquals(expectedJObject, actualJObject), "Expected and actual JSON do not match.");
        }
    }
}
