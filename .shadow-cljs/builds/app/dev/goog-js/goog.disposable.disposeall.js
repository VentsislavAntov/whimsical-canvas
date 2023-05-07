["^ ","~:resource-id",["~:shadow.build.classpath/resource","goog/disposable/disposeall.js"],"~:js","goog.loadModule(function(exports) {\n  \"use strict\";\n  goog.module(\"goog.disposeAll\");\n  goog.module.declareLegacyNamespace();\n  const dispose = goog.require(\"goog.dispose\");\n  function disposeAll(var_args) {\n    for (let i = 0, len = arguments.length; i < len; ++i) {\n      const disposable = arguments[i];\n      if (goog.isArrayLike(disposable)) {\n        disposeAll.apply(null, disposable);\n      } else {\n        dispose(disposable);\n      }\n    }\n  }\n  exports = disposeAll;\n  return exports;\n});\n","~:source","/**\n * @license\n * Copyright The Closure Library Authors.\n * SPDX-License-Identifier: Apache-2.0\n */\n\n/**\n * @fileoverview The disposeAll method is used to clean up references and\n * resources.\n */\n\ngoog.module('goog.disposeAll');\ngoog.module.declareLegacyNamespace();\n\nconst dispose = goog.require('goog.dispose');\n\n/**\n * Calls `dispose` on each member of the list that supports it. (If the\n * member is an ArrayLike, then `goog.disposeAll()` will be called\n * recursively on each of its members.) If the member is not an object with a\n * `dispose()` method, then it is ignored.\n * @param {...*} var_args The list.\n */\nfunction disposeAll(var_args) {\n  for (let i = 0, len = arguments.length; i < len; ++i) {\n    const disposable = arguments[i];\n    if (goog.isArrayLike(disposable)) {\n      disposeAll.apply(null, disposable);\n    } else {\n      dispose(disposable);\n    }\n  }\n}\nexports = disposeAll;\n","~:compiled-at",1683368285386,"~:source-map-json","{\n\"version\":3,\n\"file\":\"goog.disposable.disposeall.js\",\n\"lineCount\":19,\n\"mappings\":\"AAAA,IAAA,CAAA,UAAA,CAAA,QAAA,CAAA,OAAA,CAAA;AAAA,cAAA;AAWAA,MAAKC,CAAAA,MAAL,CAAY,iBAAZ,CAAA;AACAD,MAAKC,CAAAA,MAAOC,CAAAA,sBAAZ,EAAA;AAEA,QAAMC,UAAUH,IAAKI,CAAAA,OAAL,CAAa,cAAb,CAAhB;AASAC,UAASA,WAAU,CAACC,QAAD,CAAW;AAC5B,SAAK,IAAIC,IAAI,CAAR,EAAWC,MAAMC,SAAUC,CAAAA,MAAhC,EAAwCH,CAAxC,GAA4CC,GAA5C,EAAiD,EAAED,CAAnD,CAAsD;AACpD,YAAMI,aAAaF,SAAA,CAAUF,CAAV,CAAnB;AACA,UAAIP,IAAKY,CAAAA,WAAL,CAAiBD,UAAjB,CAAJ;AACEN,kBAAWQ,CAAAA,KAAX,CAAiB,IAAjB,EAAuBF,UAAvB,CAAA;AADF;AAGER,eAAA,CAAQQ,UAAR,CAAA;AAHF;AAFoD;AAD1B;AAU9BG,SAAA,GAAUT,UAAV;AAjCA,SAAA,OAAA;AAAA,CAAA,CAAA;;\",\n\"sources\":[\"goog/disposable/disposeall.js\"],\n\"sourcesContent\":[\"/**\\n * @license\\n * Copyright The Closure Library Authors.\\n * SPDX-License-Identifier: Apache-2.0\\n */\\n\\n/**\\n * @fileoverview The disposeAll method is used to clean up references and\\n * resources.\\n */\\n\\ngoog.module('goog.disposeAll');\\ngoog.module.declareLegacyNamespace();\\n\\nconst dispose = goog.require('goog.dispose');\\n\\n/**\\n * Calls `dispose` on each member of the list that supports it. (If the\\n * member is an ArrayLike, then `goog.disposeAll()` will be called\\n * recursively on each of its members.) If the member is not an object with a\\n * `dispose()` method, then it is ignored.\\n * @param {...*} var_args The list.\\n */\\nfunction disposeAll(var_args) {\\n  for (let i = 0, len = arguments.length; i < len; ++i) {\\n    const disposable = arguments[i];\\n    if (goog.isArrayLike(disposable)) {\\n      disposeAll.apply(null, disposable);\\n    } else {\\n      dispose(disposable);\\n    }\\n  }\\n}\\nexports = disposeAll;\\n\"],\n\"names\":[\"goog\",\"module\",\"declareLegacyNamespace\",\"dispose\",\"require\",\"disposeAll\",\"var_args\",\"i\",\"len\",\"arguments\",\"length\",\"disposable\",\"isArrayLike\",\"apply\",\"exports\"]\n}\n"]