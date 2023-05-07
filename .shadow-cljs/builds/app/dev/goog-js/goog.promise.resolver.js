["^ ","~:resource-id",["~:shadow.build.classpath/resource","goog/promise/resolver.js"],"~:js","goog.loadModule(function(exports) {\n  \"use strict\";\n  goog.module(\"goog.promise.Resolver\");\n  goog.module.declareLegacyNamespace();\n  const GoogPromise = goog.requireType(\"goog.Promise\");\n  const Thenable = goog.requireType(\"goog.Thenable\");\n  class Resolver {\n    constructor() {\n      this.promise;\n      this.resolve;\n      this.reject;\n    }\n  }\n  exports = Resolver;\n  return exports;\n});\n","~:source","/**\n * @license\n * Copyright The Closure Library Authors.\n * SPDX-License-Identifier: Apache-2.0\n */\ngoog.module('goog.promise.Resolver');\ngoog.module.declareLegacyNamespace();\n\nconst GoogPromise = goog.requireType('goog.Promise');\nconst Thenable = goog.requireType('goog.Thenable');\n\n/**\n * Resolver interface for promises. The resolver is a convenience interface that\n * bundles the promise and its associated resolve and reject functions together,\n * for cases where the resolver needs to be persisted internally.\n * @template TYPE\n * @interface\n */\nclass Resolver {\n  constructor() {\n    /**\n     * The promise that created this resolver.\n     * @type {!GoogPromise<TYPE>}\n     */\n    this.promise;\n    /**\n     * Resolves this resolver with the specified value.\n     * @type {function((TYPE|GoogPromise<TYPE>|Promise<TYPE>|IThenable|Thenable)=)}\n     */\n    this.resolve;\n    /**\n     * Rejects this resolver with the specified reason.\n     * @type {function(*=): void}\n     */\n    this.reject;\n  }\n}\n\nexports = Resolver;\n","~:compiled-at",1683368285379,"~:source-map-json","{\n\"version\":3,\n\"file\":\"goog.promise.resolver.js\",\n\"lineCount\":17,\n\"mappings\":\"AAAA,IAAA,CAAA,UAAA,CAAA,QAAA,CAAA,OAAA,CAAA;AAAA,cAAA;AAKAA,MAAKC,CAAAA,MAAL,CAAY,uBAAZ,CAAA;AACAD,MAAKC,CAAAA,MAAOC,CAAAA,sBAAZ,EAAA;AAEA,QAAMC,cAAcH,IAAKI,CAAAA,WAAL,CAAiB,cAAjB,CAApB;AACA,QAAMC,WAAWL,IAAKI,CAAAA,WAAL,CAAiB,eAAjB,CAAjB;AASA,OAAME,SAAN;AACEC,eAAW,EAAG;AAKZ,UAAKC,CAAAA,OAAL;AAKA,UAAKC,CAAAA,OAAL;AAKA,UAAKC,CAAAA,MAAL;AAfY;AADhB;AAoBAC,SAAA,GAAUL,QAAV;AAtCA,SAAA,OAAA;AAAA,CAAA,CAAA;;\",\n\"sources\":[\"goog/promise/resolver.js\"],\n\"sourcesContent\":[\"/**\\n * @license\\n * Copyright The Closure Library Authors.\\n * SPDX-License-Identifier: Apache-2.0\\n */\\ngoog.module('goog.promise.Resolver');\\ngoog.module.declareLegacyNamespace();\\n\\nconst GoogPromise = goog.requireType('goog.Promise');\\nconst Thenable = goog.requireType('goog.Thenable');\\n\\n/**\\n * Resolver interface for promises. The resolver is a convenience interface that\\n * bundles the promise and its associated resolve and reject functions together,\\n * for cases where the resolver needs to be persisted internally.\\n * @template TYPE\\n * @interface\\n */\\nclass Resolver {\\n  constructor() {\\n    /**\\n     * The promise that created this resolver.\\n     * @type {!GoogPromise<TYPE>}\\n     */\\n    this.promise;\\n    /**\\n     * Resolves this resolver with the specified value.\\n     * @type {function((TYPE|GoogPromise<TYPE>|Promise<TYPE>|IThenable|Thenable)=)}\\n     */\\n    this.resolve;\\n    /**\\n     * Rejects this resolver with the specified reason.\\n     * @type {function(*=): void}\\n     */\\n    this.reject;\\n  }\\n}\\n\\nexports = Resolver;\\n\"],\n\"names\":[\"goog\",\"module\",\"declareLegacyNamespace\",\"GoogPromise\",\"requireType\",\"Thenable\",\"Resolver\",\"constructor\",\"promise\",\"resolve\",\"reject\",\"exports\"]\n}\n"]