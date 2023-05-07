["^ ","~:resource-id",["~:shadow.build.classpath/resource","goog/labs/useragent/platform.js"],"~:js","goog.loadModule(function(exports) {\n  \"use strict\";\n  goog.module(\"goog.labs.userAgent.platform\");\n  goog.module.declareLegacyNamespace();\n  const googString = goog.require(\"goog.string.internal\");\n  const util = goog.require(\"goog.labs.userAgent.util\");\n  const {AsyncValue, Version} = goog.require(\"goog.labs.userAgent.highEntropy.highEntropyValue\");\n  const {platformVersion} = goog.require(\"goog.labs.userAgent.highEntropy.highEntropyData\");\n  const {useClientHints} = goog.require(\"goog.labs.userAgent\");\n  function useUserAgentDataPlatform(ignoreClientHintsFlag = false) {\n    if (util.ASSUME_CLIENT_HINTS_SUPPORT) {\n      return true;\n    }\n    if (!ignoreClientHintsFlag && !useClientHints()) {\n      return false;\n    }\n    const userAgentData = util.getUserAgentData();\n    return !!userAgentData && !!userAgentData.platform;\n  }\n  function isAndroid() {\n    if (useUserAgentDataPlatform()) {\n      return util.getUserAgentData().platform === \"Android\";\n    }\n    return util.matchUserAgent(\"Android\");\n  }\n  function isIpod() {\n    return util.matchUserAgent(\"iPod\");\n  }\n  function isIphone() {\n    return util.matchUserAgent(\"iPhone\") && !util.matchUserAgent(\"iPod\") && !util.matchUserAgent(\"iPad\");\n  }\n  function isIpad() {\n    return util.matchUserAgent(\"iPad\");\n  }\n  function isIos() {\n    return isIphone() || isIpad() || isIpod();\n  }\n  function isMacintosh() {\n    if (useUserAgentDataPlatform()) {\n      return util.getUserAgentData().platform === \"macOS\";\n    }\n    return util.matchUserAgent(\"Macintosh\");\n  }\n  function isLinux() {\n    if (useUserAgentDataPlatform()) {\n      return util.getUserAgentData().platform === \"Linux\";\n    }\n    return util.matchUserAgent(\"Linux\");\n  }\n  function isWindows() {\n    if (useUserAgentDataPlatform()) {\n      return util.getUserAgentData().platform === \"Windows\";\n    }\n    return util.matchUserAgent(\"Windows\");\n  }\n  function isChromeOS() {\n    if (useUserAgentDataPlatform()) {\n      return util.getUserAgentData().platform === \"Chrome OS\";\n    }\n    return util.matchUserAgent(\"CrOS\");\n  }\n  function isChromecast() {\n    return util.matchUserAgent(\"CrKey\");\n  }\n  function isKaiOS() {\n    return util.matchUserAgentIgnoreCase(\"KaiOS\");\n  }\n  function getVersion() {\n    const userAgentString = util.getUserAgent();\n    let version = \"\", re;\n    if (isWindows()) {\n      re = /Windows (?:NT|Phone) ([0-9.]+)/;\n      const match = re.exec(userAgentString);\n      if (match) {\n        version = match[1];\n      } else {\n        version = \"0.0\";\n      }\n    } else if (isIos()) {\n      re = /(?:iPhone|iPod|iPad|CPU)\\s+OS\\s+(\\S+)/;\n      const match = re.exec(userAgentString);\n      version = match && match[1].replace(/_/g, \".\");\n    } else if (isMacintosh()) {\n      re = /Mac OS X ([0-9_.]+)/;\n      const match = re.exec(userAgentString);\n      version = match ? match[1].replace(/_/g, \".\") : \"10\";\n    } else if (isKaiOS()) {\n      re = /(?:KaiOS)\\/(\\S+)/i;\n      const match = re.exec(userAgentString);\n      version = match && match[1];\n    } else if (isAndroid()) {\n      re = /Android\\s+([^\\);]+)(\\)|;)/;\n      const match = re.exec(userAgentString);\n      version = match && match[1];\n    } else if (isChromeOS()) {\n      re = /(?:CrOS\\s+(?:i686|x86_64)\\s+([0-9.]+))/;\n      const match = re.exec(userAgentString);\n      version = match && match[1];\n    }\n    return version || \"\";\n  }\n  function isVersionOrHigher(version) {\n    return googString.compareVersions(getVersion(), version) >= 0;\n  }\n  class PlatformVersion {\n    constructor() {\n      this.preUachHasLoaded_ = false;\n    }\n    getIfLoaded() {\n      if (useUserAgentDataPlatform(true)) {\n        const loadedPlatformVersion = platformVersion.getIfLoaded();\n        if (loadedPlatformVersion === undefined) {\n          return undefined;\n        }\n        return new Version(loadedPlatformVersion);\n      } else if (!this.preUachHasLoaded_) {\n        return undefined;\n      } else {\n        return new Version(getVersion());\n      }\n    }\n    async load() {\n      if (useUserAgentDataPlatform(true)) {\n        return new Version(await platformVersion.load());\n      } else {\n        this.preUachHasLoaded_ = true;\n        return new Version(getVersion());\n      }\n    }\n    resetForTesting() {\n      platformVersion.resetForTesting();\n      this.preUachHasLoaded_ = false;\n    }\n  }\n  const version = new PlatformVersion();\n  exports = {getVersion, isAndroid, isChromeOS, isChromecast, isIos, isIpad, isIphone, isIpod, isKaiOS, isLinux, isMacintosh, isVersionOrHigher, isWindows, version,};\n  return exports;\n});\n","~:source","/**\n * @license\n * Copyright The Closure Library Authors.\n * SPDX-License-Identifier: Apache-2.0\n */\n\n/**\n * @fileoverview Closure user agent platform detection.\n * @see <a href=\"http://www.useragentstring.com/\">User agent strings</a>\n * For more information on browser brand, rendering engine, or device see the\n * other sub-namespaces in goog.labs.userAgent (browser, engine, and device\n * respectively).\n */\n\ngoog.module('goog.labs.userAgent.platform');\ngoog.module.declareLegacyNamespace();\n\nconst googString = goog.require('goog.string.internal');\nconst util = goog.require('goog.labs.userAgent.util');\nconst {AsyncValue, Version} = goog.require('goog.labs.userAgent.highEntropy.highEntropyValue');\nconst {platformVersion} = goog.require('goog.labs.userAgent.highEntropy.highEntropyData');\nconst {useClientHints} = goog.require('goog.labs.userAgent');\n\n/**\n * @param {boolean=} ignoreClientHintsFlag Iff truthy, the `useClientHints`\n *     function will not be called when evaluating if User-Agent Client Hints\n *     Brand data can be used. For existing labs.userAgent API surfaces with\n *     widespread use, this should be a falsy value so that usage of the Client\n *     Hints APIs can be gated behind flags / experiment rollouts.\n * @return {boolean} Whether to use navigator.userAgentData to determine\n * the current platform.\n * userAgentData.platform was enabled by default in Chrome 93:\n * https://www.chromestatus.com/feature/5733498725859328\n * TODO(user): Skip this check with FEATURESET_YEAR once userAgentData is\n * present in all major browsers (may not be until 2024).\n * See https://caniuse.com/mdn-api_navigator_useragentdata.\n */\nfunction useUserAgentDataPlatform(ignoreClientHintsFlag = false) {\n  if (util.ASSUME_CLIENT_HINTS_SUPPORT) return true;\n  // High-entropy API surfaces should not be gated behind the useClientHints\n  // check (as in production it is gated behind a define).\n  if (!ignoreClientHintsFlag && !useClientHints()) return false;\n  const userAgentData = util.getUserAgentData();\n  return !!userAgentData && !!userAgentData.platform;\n}\n\n/**\n * @return {boolean} Whether the platform is Android.\n */\nfunction isAndroid() {\n  if (useUserAgentDataPlatform()) {\n    return util.getUserAgentData().platform === 'Android';\n  }\n  return util.matchUserAgent('Android');\n}\n\n/**\n * @return {boolean} Whether the platform is iPod.\n * TODO(user): Combine iPod/iPhone detection since they may become\n * indistinguishable if we begin relying on userAgentdata in iOS.\n */\nfunction isIpod() {\n  // navigator.userAgentData is currently not supported on any iOS browser, so\n  // rely only on navigator.userAgent.\n  return util.matchUserAgent('iPod');\n}\n\n/**\n * @return {boolean} Whether the platform is iPhone.\n */\nfunction isIphone() {\n  // navigator.userAgentData is currently not supported on any iOS browser, so\n  // rely only on navigator.userAgent.\n  return util.matchUserAgent('iPhone') && !util.matchUserAgent('iPod') &&\n      !util.matchUserAgent('iPad');\n}\n\n/**\n * Returns whether the platform is iPad.\n * Note that iPadOS 13+ spoofs macOS Safari by default in its user agent, and in\n * this scenario the platform will not be recognized as iPad. If you must have\n * iPad-specific behavior, use\n * {@link goog.labs.userAgent.extra.isSafariDesktopOnMobile}.\n * @return {boolean} Whether the platform is iPad.\n */\nfunction isIpad() {\n  // navigator.userAgentData is currently not supported on any iOS browser, so\n  // rely only on navigator.userAgent.\n  return util.matchUserAgent('iPad');\n}\n\n/**\n * Returns whether the platform is iOS.\n * Note that iPadOS 13+ spoofs macOS Safari by default in its user agent, and in\n * this scenario the platform will not be recognized as iOS. If you must have\n * iPad-specific behavior, use\n * {@link goog.labs.userAgent.extra.isSafariDesktopOnMobile}.\n * @return {boolean} Whether the platform is iOS.\n */\nfunction isIos() {\n  return isIphone() || isIpad() || isIpod();\n}\n\n/**\n * @return {boolean} Whether the platform is Mac.\n */\nfunction isMacintosh() {\n  if (useUserAgentDataPlatform()) {\n    return util.getUserAgentData().platform === 'macOS';\n  }\n  return util.matchUserAgent('Macintosh');\n}\n\n/**\n * Note: ChromeOS is not considered to be Linux as it does not report itself\n * as Linux in the user agent string.\n * @return {boolean} Whether the platform is Linux.\n */\nfunction isLinux() {\n  if (useUserAgentDataPlatform()) {\n    return util.getUserAgentData().platform === 'Linux';\n  }\n  return util.matchUserAgent('Linux');\n}\n\n/**\n * @return {boolean} Whether the platform is Windows.\n */\nfunction isWindows() {\n  if (useUserAgentDataPlatform()) {\n    return util.getUserAgentData().platform === 'Windows';\n  }\n  return util.matchUserAgent('Windows');\n}\n\n/**\n * @return {boolean} Whether the platform is ChromeOS.\n */\nfunction isChromeOS() {\n  if (useUserAgentDataPlatform()) {\n    return util.getUserAgentData().platform === 'Chrome OS';\n  }\n  return util.matchUserAgent('CrOS');\n}\n\n/**\n * @return {boolean} Whether the platform is Chromecast.\n */\nfunction isChromecast() {\n  // TODO(user): Check against util.getUserAgentData().platform once the\n  // OS string for Chromecast is known.\n  return util.matchUserAgent('CrKey');\n}\n\n/**\n * @return {boolean} Whether the platform is KaiOS.\n */\nfunction isKaiOS() {\n  // navigator.userAgentData is currently not supported on any KaiOS browser, so\n  // rely only on navigator.userAgent.\n  return util.matchUserAgentIgnoreCase('KaiOS');\n}\n\n/**\n * The version of the platform. We only determine the version for Windows,\n * Mac, and Chrome OS. It doesn't make much sense on Linux. For Windows, we only\n * look at the NT version. Non-NT-based versions (e.g. 95, 98, etc.) are given\n * version 0.0.\n *\n * @return {string} The platform version or empty string if version cannot be\n *     determined.\n */\nfunction getVersion() {\n  const userAgentString = util.getUserAgent();\n  let version = '', re;\n  if (isWindows()) {\n    re = /Windows (?:NT|Phone) ([0-9.]+)/;\n    const match = re.exec(userAgentString);\n    if (match) {\n      version = match[1];\n    } else {\n      version = '0.0';\n    }\n  } else if (isIos()) {\n    re = /(?:iPhone|iPod|iPad|CPU)\\s+OS\\s+(\\S+)/;\n    const match = re.exec(userAgentString);\n    // Report the version as x.y.z and not x_y_z\n    version = match && match[1].replace(/_/g, '.');\n  } else if (isMacintosh()) {\n    re = /Mac OS X ([0-9_.]+)/;\n    const match = re.exec(userAgentString);\n    // Note: some old versions of Camino do not report an OSX version.\n    // Default to 10.\n    version = match ? match[1].replace(/_/g, '.') : '10';\n  } else if (isKaiOS()) {\n    re = /(?:KaiOS)\\/(\\S+)/i;\n    const match = re.exec(userAgentString);\n    version = match && match[1];\n  } else if (isAndroid()) {\n    re = /Android\\s+([^\\);]+)(\\)|;)/;\n    const match = re.exec(userAgentString);\n    version = match && match[1];\n  } else if (isChromeOS()) {\n    re = /(?:CrOS\\s+(?:i686|x86_64)\\s+([0-9.]+))/;\n    const match = re.exec(userAgentString);\n    version = match && match[1];\n  }\n  return version || '';\n}\n\n/**\n * @param {string|number} version The version to check.\n * @return {boolean} Whether the browser version is higher or the same as the\n *     given version.\n */\nfunction isVersionOrHigher(version) {\n  return googString.compareVersions(getVersion(), version) >= 0;\n}\n\n/**\n * Represents a high-entropy version string.\n * @implements {AsyncValue<!Version>}\n */\nclass PlatformVersion {\n  constructor() {\n    /** @private {boolean} */\n    this.preUachHasLoaded_ = false;\n  }\n\n  /**\n   * @return {!Version|undefined}\n   * @override\n   */\n  getIfLoaded() {\n    if (useUserAgentDataPlatform(true)) {\n      const loadedPlatformVersion = platformVersion.getIfLoaded();\n      if (loadedPlatformVersion === undefined) {\n        // No platform data has been cached\n        return undefined;\n      }\n      return new Version(loadedPlatformVersion);\n    } else if (!this.preUachHasLoaded_) {\n      // Nobody ever called `load` on this class instance, so we should return\n      // nothing to match the semantics of the class when using the Client Hint\n      // APIs.\n      return undefined;\n    } else {\n      // `load` has been called, so we can return a Version derived from the\n      // useragent string.\n      return new Version(getVersion());\n    }\n  }\n\n  /**\n   * @return {!Promise<!Version>}\n   * @override\n   */\n  async load() {\n    if (useUserAgentDataPlatform(true)) {\n      return new Version(await platformVersion.load());\n    } else {\n      this.preUachHasLoaded_ = true;\n      return new Version(getVersion());\n    }\n  }\n\n  /** @package */\n  resetForTesting() {\n    platformVersion.resetForTesting();\n    this.preUachHasLoaded_ = false;\n  }\n}\n\n/**\n * The platform version, a high-entropy value.\n * @type {!PlatformVersion}\n */\nconst version = new PlatformVersion();\n\nexports = {\n  getVersion,\n  isAndroid,\n  isChromeOS,\n  isChromecast,\n  isIos,\n  isIpad,\n  isIphone,\n  isIpod,\n  isKaiOS,\n  isLinux,\n  isMacintosh,\n  isVersionOrHigher,\n  isWindows,\n  version,\n};\n","~:compiled-at",1683368284995,"~:source-map-json","{\n\"version\":3,\n\"file\":\"goog.labs.useragent.platform.js\",\n\"lineCount\":139,\n\"mappings\":\"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\",\n\"sources\":[\"goog/labs/useragent/platform.js\"],\n\"sourcesContent\":[\"/**\\n * @license\\n * Copyright The Closure Library Authors.\\n * SPDX-License-Identifier: Apache-2.0\\n */\\n\\n/**\\n * @fileoverview Closure user agent platform detection.\\n * @see <a href=\\\"http://www.useragentstring.com/\\\">User agent strings</a>\\n * For more information on browser brand, rendering engine, or device see the\\n * other sub-namespaces in goog.labs.userAgent (browser, engine, and device\\n * respectively).\\n */\\n\\ngoog.module('goog.labs.userAgent.platform');\\ngoog.module.declareLegacyNamespace();\\n\\nconst googString = goog.require('goog.string.internal');\\nconst util = goog.require('goog.labs.userAgent.util');\\nconst {AsyncValue, Version} = goog.require('goog.labs.userAgent.highEntropy.highEntropyValue');\\nconst {platformVersion} = goog.require('goog.labs.userAgent.highEntropy.highEntropyData');\\nconst {useClientHints} = goog.require('goog.labs.userAgent');\\n\\n/**\\n * @param {boolean=} ignoreClientHintsFlag Iff truthy, the `useClientHints`\\n *     function will not be called when evaluating if User-Agent Client Hints\\n *     Brand data can be used. For existing labs.userAgent API surfaces with\\n *     widespread use, this should be a falsy value so that usage of the Client\\n *     Hints APIs can be gated behind flags / experiment rollouts.\\n * @return {boolean} Whether to use navigator.userAgentData to determine\\n * the current platform.\\n * userAgentData.platform was enabled by default in Chrome 93:\\n * https://www.chromestatus.com/feature/5733498725859328\\n * TODO(user): Skip this check with FEATURESET_YEAR once userAgentData is\\n * present in all major browsers (may not be until 2024).\\n * See https://caniuse.com/mdn-api_navigator_useragentdata.\\n */\\nfunction useUserAgentDataPlatform(ignoreClientHintsFlag = false) {\\n  if (util.ASSUME_CLIENT_HINTS_SUPPORT) return true;\\n  // High-entropy API surfaces should not be gated behind the useClientHints\\n  // check (as in production it is gated behind a define).\\n  if (!ignoreClientHintsFlag && !useClientHints()) return false;\\n  const userAgentData = util.getUserAgentData();\\n  return !!userAgentData && !!userAgentData.platform;\\n}\\n\\n/**\\n * @return {boolean} Whether the platform is Android.\\n */\\nfunction isAndroid() {\\n  if (useUserAgentDataPlatform()) {\\n    return util.getUserAgentData().platform === 'Android';\\n  }\\n  return util.matchUserAgent('Android');\\n}\\n\\n/**\\n * @return {boolean} Whether the platform is iPod.\\n * TODO(user): Combine iPod/iPhone detection since they may become\\n * indistinguishable if we begin relying on userAgentdata in iOS.\\n */\\nfunction isIpod() {\\n  // navigator.userAgentData is currently not supported on any iOS browser, so\\n  // rely only on navigator.userAgent.\\n  return util.matchUserAgent('iPod');\\n}\\n\\n/**\\n * @return {boolean} Whether the platform is iPhone.\\n */\\nfunction isIphone() {\\n  // navigator.userAgentData is currently not supported on any iOS browser, so\\n  // rely only on navigator.userAgent.\\n  return util.matchUserAgent('iPhone') && !util.matchUserAgent('iPod') &&\\n      !util.matchUserAgent('iPad');\\n}\\n\\n/**\\n * Returns whether the platform is iPad.\\n * Note that iPadOS 13+ spoofs macOS Safari by default in its user agent, and in\\n * this scenario the platform will not be recognized as iPad. If you must have\\n * iPad-specific behavior, use\\n * {@link goog.labs.userAgent.extra.isSafariDesktopOnMobile}.\\n * @return {boolean} Whether the platform is iPad.\\n */\\nfunction isIpad() {\\n  // navigator.userAgentData is currently not supported on any iOS browser, so\\n  // rely only on navigator.userAgent.\\n  return util.matchUserAgent('iPad');\\n}\\n\\n/**\\n * Returns whether the platform is iOS.\\n * Note that iPadOS 13+ spoofs macOS Safari by default in its user agent, and in\\n * this scenario the platform will not be recognized as iOS. If you must have\\n * iPad-specific behavior, use\\n * {@link goog.labs.userAgent.extra.isSafariDesktopOnMobile}.\\n * @return {boolean} Whether the platform is iOS.\\n */\\nfunction isIos() {\\n  return isIphone() || isIpad() || isIpod();\\n}\\n\\n/**\\n * @return {boolean} Whether the platform is Mac.\\n */\\nfunction isMacintosh() {\\n  if (useUserAgentDataPlatform()) {\\n    return util.getUserAgentData().platform === 'macOS';\\n  }\\n  return util.matchUserAgent('Macintosh');\\n}\\n\\n/**\\n * Note: ChromeOS is not considered to be Linux as it does not report itself\\n * as Linux in the user agent string.\\n * @return {boolean} Whether the platform is Linux.\\n */\\nfunction isLinux() {\\n  if (useUserAgentDataPlatform()) {\\n    return util.getUserAgentData().platform === 'Linux';\\n  }\\n  return util.matchUserAgent('Linux');\\n}\\n\\n/**\\n * @return {boolean} Whether the platform is Windows.\\n */\\nfunction isWindows() {\\n  if (useUserAgentDataPlatform()) {\\n    return util.getUserAgentData().platform === 'Windows';\\n  }\\n  return util.matchUserAgent('Windows');\\n}\\n\\n/**\\n * @return {boolean} Whether the platform is ChromeOS.\\n */\\nfunction isChromeOS() {\\n  if (useUserAgentDataPlatform()) {\\n    return util.getUserAgentData().platform === 'Chrome OS';\\n  }\\n  return util.matchUserAgent('CrOS');\\n}\\n\\n/**\\n * @return {boolean} Whether the platform is Chromecast.\\n */\\nfunction isChromecast() {\\n  // TODO(user): Check against util.getUserAgentData().platform once the\\n  // OS string for Chromecast is known.\\n  return util.matchUserAgent('CrKey');\\n}\\n\\n/**\\n * @return {boolean} Whether the platform is KaiOS.\\n */\\nfunction isKaiOS() {\\n  // navigator.userAgentData is currently not supported on any KaiOS browser, so\\n  // rely only on navigator.userAgent.\\n  return util.matchUserAgentIgnoreCase('KaiOS');\\n}\\n\\n/**\\n * The version of the platform. We only determine the version for Windows,\\n * Mac, and Chrome OS. It doesn't make much sense on Linux. For Windows, we only\\n * look at the NT version. Non-NT-based versions (e.g. 95, 98, etc.) are given\\n * version 0.0.\\n *\\n * @return {string} The platform version or empty string if version cannot be\\n *     determined.\\n */\\nfunction getVersion() {\\n  const userAgentString = util.getUserAgent();\\n  let version = '', re;\\n  if (isWindows()) {\\n    re = /Windows (?:NT|Phone) ([0-9.]+)/;\\n    const match = re.exec(userAgentString);\\n    if (match) {\\n      version = match[1];\\n    } else {\\n      version = '0.0';\\n    }\\n  } else if (isIos()) {\\n    re = /(?:iPhone|iPod|iPad|CPU)\\\\s+OS\\\\s+(\\\\S+)/;\\n    const match = re.exec(userAgentString);\\n    // Report the version as x.y.z and not x_y_z\\n    version = match && match[1].replace(/_/g, '.');\\n  } else if (isMacintosh()) {\\n    re = /Mac OS X ([0-9_.]+)/;\\n    const match = re.exec(userAgentString);\\n    // Note: some old versions of Camino do not report an OSX version.\\n    // Default to 10.\\n    version = match ? match[1].replace(/_/g, '.') : '10';\\n  } else if (isKaiOS()) {\\n    re = /(?:KaiOS)\\\\/(\\\\S+)/i;\\n    const match = re.exec(userAgentString);\\n    version = match && match[1];\\n  } else if (isAndroid()) {\\n    re = /Android\\\\s+([^\\\\);]+)(\\\\)|;)/;\\n    const match = re.exec(userAgentString);\\n    version = match && match[1];\\n  } else if (isChromeOS()) {\\n    re = /(?:CrOS\\\\s+(?:i686|x86_64)\\\\s+([0-9.]+))/;\\n    const match = re.exec(userAgentString);\\n    version = match && match[1];\\n  }\\n  return version || '';\\n}\\n\\n/**\\n * @param {string|number} version The version to check.\\n * @return {boolean} Whether the browser version is higher or the same as the\\n *     given version.\\n */\\nfunction isVersionOrHigher(version) {\\n  return googString.compareVersions(getVersion(), version) >= 0;\\n}\\n\\n/**\\n * Represents a high-entropy version string.\\n * @implements {AsyncValue<!Version>}\\n */\\nclass PlatformVersion {\\n  constructor() {\\n    /** @private {boolean} */\\n    this.preUachHasLoaded_ = false;\\n  }\\n\\n  /**\\n   * @return {!Version|undefined}\\n   * @override\\n   */\\n  getIfLoaded() {\\n    if (useUserAgentDataPlatform(true)) {\\n      const loadedPlatformVersion = platformVersion.getIfLoaded();\\n      if (loadedPlatformVersion === undefined) {\\n        // No platform data has been cached\\n        return undefined;\\n      }\\n      return new Version(loadedPlatformVersion);\\n    } else if (!this.preUachHasLoaded_) {\\n      // Nobody ever called `load` on this class instance, so we should return\\n      // nothing to match the semantics of the class when using the Client Hint\\n      // APIs.\\n      return undefined;\\n    } else {\\n      // `load` has been called, so we can return a Version derived from the\\n      // useragent string.\\n      return new Version(getVersion());\\n    }\\n  }\\n\\n  /**\\n   * @return {!Promise<!Version>}\\n   * @override\\n   */\\n  async load() {\\n    if (useUserAgentDataPlatform(true)) {\\n      return new Version(await platformVersion.load());\\n    } else {\\n      this.preUachHasLoaded_ = true;\\n      return new Version(getVersion());\\n    }\\n  }\\n\\n  /** @package */\\n  resetForTesting() {\\n    platformVersion.resetForTesting();\\n    this.preUachHasLoaded_ = false;\\n  }\\n}\\n\\n/**\\n * The platform version, a high-entropy value.\\n * @type {!PlatformVersion}\\n */\\nconst version = new PlatformVersion();\\n\\nexports = {\\n  getVersion,\\n  isAndroid,\\n  isChromeOS,\\n  isChromecast,\\n  isIos,\\n  isIpad,\\n  isIphone,\\n  isIpod,\\n  isKaiOS,\\n  isLinux,\\n  isMacintosh,\\n  isVersionOrHigher,\\n  isWindows,\\n  version,\\n};\\n\"],\n\"names\":[\"goog\",\"module\",\"declareLegacyNamespace\",\"googString\",\"require\",\"util\",\"AsyncValue\",\"Version\",\"platformVersion\",\"useClientHints\",\"useUserAgentDataPlatform\",\"ignoreClientHintsFlag\",\"ASSUME_CLIENT_HINTS_SUPPORT\",\"userAgentData\",\"getUserAgentData\",\"platform\",\"isAndroid\",\"matchUserAgent\",\"isIpod\",\"isIphone\",\"isIpad\",\"isIos\",\"isMacintosh\",\"isLinux\",\"isWindows\",\"isChromeOS\",\"isChromecast\",\"isKaiOS\",\"matchUserAgentIgnoreCase\",\"getVersion\",\"userAgentString\",\"getUserAgent\",\"version\",\"re\",\"match\",\"exec\",\"replace\",\"isVersionOrHigher\",\"compareVersions\",\"PlatformVersion\",\"constructor\",\"preUachHasLoaded_\",\"getIfLoaded\",\"loadedPlatformVersion\",\"undefined\",\"load\",\"resetForTesting\",\"exports\"]\n}\n"]