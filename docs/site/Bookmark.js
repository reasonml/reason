/*!
 * Flatdoc - (c) 2013, 2014 Rico Sta. Cruz
 * http://ricostacruz.com/flatdoc
 * @license MIT
 */


// Keep this in sync with $header-height in style.
var headerHeight = 52;

/**
 * The default searchCrumbContext function. Clears out any h1 context if there
 * is an h2. TODO: only clear out h1 if there is an h2 and there is only *one*
 * h1.
 */
var defaultSearchBreadcrumbContext = function(ctx) {
  return ctx;
  // If there *is* an h2+, don't show h1
  // if(ctx.h2 || ctx.h3 || ctx.h4) {
  //   return {...ctx, h1: null};
  // } else {
  //   // Else show the h1
  //   return ctx;
  // }
};
/**
 * Pass the window.location.href
 */
var urlBasename = function(s) {
  return s.split('/').pop().split('#')[0].split('?')[0];
};
var urlExtensionlessBasename = function(s) {
  return s.split('/').pop().split('#')[0].split('?')[0].replace(".html", "");
};

var slugPrefix = function(hash) {
  if (hash === '' || hash[0] !== '#') {
    return ''
  } else {
    hash = hash.substr(1);
    return (hash.split('-').length ? hash.split('-')[0] : '').toLowerCase();
  }
};

var pageKeyForUrl = function(windowLocation, pageConfig) {
  var urlBasenameRoot = urlBasename(windowLocation.pathname).replace(".html", "").replace(".htm", "").toLowerCase().toLowerCase();
  for(var pageKey in pageConfig) {
    if(urlBasenameRoot.toLowerCase() === pageKey) {
      return pageKey.toLowerCase();
    }
  }
  return null;
};

var getEffectivePageKeyAndHashFromPageifiedId = function(id) {
  if(hash.lastIndexOf('#') === -1) {
    console.error('cannot parse pageified id', id);
    return {
      pageKey: null,
      hashContents: id
    };
  } else {
    var effectivePageKey =
      hash.substr(0, hash.lastIndexOf('#') - 1).replace(".html", "").replace(".htm", "").toLowerCase();
    return {
      pageKey: effectivePageKey,
      hashContents: hash.substr(hash.lastIndexOf('#') + 1)

    }
  }
};

/**
 * Urls like blah.html#foo/bar#another-hash
 * Are interpreted as being another way to reference the page
 * foo/bar.html#another-hash (Currently everything relative from the
 * timeTemplate)
 */
var getEffectivePageAndHashFromLocation = function(windowLocation) {
  var urlBasenameRootLowerCase =
    urlBasename(windowLocation.pathname).replace(".html", "").replace(".htm", "").toLowerCase().toLowerCase();
  var hash = windowLocation.hash;
  if(hash[0] !== '#' || (hash[0] === '#' && hash.lastIndexOf('#') === 0)) {
    return {
      loadingFromPageKey: urlBasenameRootLowerCase,
      pageKey: urlBasenameRootLowerCase,
      hashContents: hash.substr(1)
    };
  } else {
    var effectivePageKey =
      hash.substr(1, hash.lastIndexOf('#') - 1).replace(".html", "").replace(".htm", "").toLowerCase();
    return {
      loadingFromPageKey: urlBasenameRootLowerCase,
      pageKey: effectivePageKey,
      hashContents: hash.substr(hash.lastIndexOf('#') + 1)

    }
  }
};


var isNodeSearchHit = function(node) {
  return (
    node.tagName === 'TR' || node.tagName === 'tr' ||
    node.tagName === 'H0' || node.tagName === 'h0' ||
    node.tagName === 'H1' || node.tagName === 'h1' ||
    node.tagName === 'H2' || node.tagName === 'h2' ||
    node.tagName === 'H3' || node.tagName === 'h3' ||
    node.tagName === 'H4' || node.tagName === 'h4' ||
    node.tagName === 'H5' || node.tagName === 'h5' ||
    node.tagName === 'H6' || node.tagName === 'h6' ||
    node.tagName === 'codetabbutton' || node.tagName === 'CODETABBUTTON' ||
    node.tagName === 'P' || node.tagName === 'p' ||
    node.tagName === 'LI' || node.tagName === 'li' ||
    node.tagName === 'UL' || node.tagName === 'ul' ||
    node.tagName === 'CODE' || node.tagName === 'code' ||
    node.tagName === 'PRE' || node.tagName === 'pre' ||
    node.nodeType === Node.TEXT_NODE
  );
};

var pageDataForUrl = function(windowLocation, pageConfig) {
  var effectivePageKeyAndHash = getEffectivePageAndHashFromLocation(windowLocation, pageConfig);
  if(!pageConfig[effectivePageKeyAndHash.pageKey]) {
    console.error(
      "Current effective page basename ",
      effectivePageKeyAndHash.pageKey,
      "is not in pageConfig",
      pageConfig
    );
    return null;
  }
  return pageConfig[effectivePageKeyAndHash.pageKey];
};


var SUPPORTS_SEARCH_TABBING = false;

/**
 * We can't have the ids of elements be the exact same as the hashes in the URL
 * because that will cause the browser to scroll. But we want to have full
 * control over scroll for things like better back button support and deep
 * linking / custom animation.
 * So the element to scroll to would have id="--bookmark-linkified--foo", but
 * the anchor links that jump to it would have href="#foo".
 *
 * This allows deep linking to page#section-header?text=this%20text Which will
 * animate a scroll to a specific text portion of that section with an
 * animation.  If we don't have full control over the animation, then our own
 * animation might fight the browser's.
 */
var BOOKMARK_LINK_ID_PREFIX = '--bookmark-linkified--';


/**
 * Prepends the linkified prefix.
 */
function linkifidIdForHash(s) {
  return BOOKMARK_LINK_ID_PREFIX + s;
}

function pageifiedIdForHash(slug, pageKey) {
  return pageKey + '#' + slug;
}


/**
 * Strips the linkified prefix.
 */
function hashForLinkifiedId(s) {
 return s.indexOf(BOOKMARK_LINK_ID_PREFIX) === 0 ?
    s.substring(BOOKMARK_LINK_ID_PREFIX.length) : s;
}


var queryContentsViaIframe = function(url, onDoneCell, onFailCell) {
  var timeout = window.setTimeout(function() {
    onFailCell.contents && onFailCell.contents(
      "Timed out loading " + url +
      ". Maybe it doesn't exist? Alternatively, perhaps you were paused " +
      "in the debugger so it timed out?"
    );
  }, 900);
  var listenerID = window.addEventListener('message', function(e) {
    if(e.data.messageType === 'docPageContent' && e.data.iframeName === url) {
      window.removeEventListener('message', listenerID);
      if(onDoneCell.contents) {
        window.clearTimeout(timeout);
        onDoneCell.contents(e.data.content);
      }
    }
  });
  var iframe = document.createElement('iframe');
  iframe.name = url;
  // Themes may opt to handle offline/pre rendering, and this is convenient
  // to mark these iframes as not-essential once rendered so they may be
  // removed from the DOM after rendering, and won't take up space in the
  // bundle.
  // TODO: Consider this for merging many html pages into one book https://github.com/fidian/metalsmith-bookify-html
  iframe.className = 'removeFromRenderedPage';
  iframe.src=url + '?bookmarkContentQuery=true';
  iframe.style="display:none !important";
  iframe.type="text/plain";
  iframe.onerror = function(e) {
    if(onFailCell.contents) {
      onFailCell.contents(e);
    }
  };
  // iframe.onload = function(e) {
  // };
  document.body.appendChild(iframe);
};

function anchorJump(href) {
  var queryParamLoc = href.indexOf('?');
  if(queryParamLoc !== -1) {
    href = href.substring(0, queryParamLoc);
  }
  if (href != '#') {
    var $area = $(href);
    console.log('href area', href, $area);
    // Find the parent
    if (!$area.length) { return; }
    customScrollIntoView({
      smooth: true,
      container: 'page',
      element: $area[0],
      mode: 'top',
      topMargin: 2 * headerHeight,
      bottomMargin: 0
    });
    $.highlightNode($area[0]);
    $('body').trigger('anchor', href);
  }
};

// https://stackoverflow.com/a/8342709
var customScrollIntoView = function(props) {
  var smooth = props.smooth || false;
  var container = props.container;
  var containerElement = props.container === 'page' ?
    document.documentElement : props.container;
  var scrollerElement = props.container === 'page' ?  window : containerElement;
  var element = props.element;
  // closest-if-needed | top | bottom
  var mode = props.mode || 'closest-if-needed';
  var topMargin = props.topMargin || 0;
  var bottomMargin = props.bottomMargin || 0;
  var containerRect = containerElement.getBoundingClientRect();
  var elementRect = element.getBoundingClientRect();
  var containerOffset = $(containerElement).offset();
  var elementOffset = $(element).offset();
  // TODO: For "whole document" scrolling,
  // use Math.max(window.pageYOffset, document.documentElement.scrollTop, document.body.scrollTop)
  // When loading the page from entrypoint mode, the document.documentElement scrollTop is zero!!
  // But not when loading form an index.dev.html. Something about the way loading from entrypoint
  // rewrites the entire document with document.write screws up the scroll measurement.
  if(mode !== 'top' && mode !== 'closest-if-needed' && mode !== 'bottom') {
    console.error('Invalid mode to scrollIntoView', mode);
  }
  var containerScrollTop =
    container === 'page' ? Math.max(window.pageYOffset, document.documentElement.scrollTop, document.body.scrollTop) : containerElement.scrollTop;
  var elementOffsetInContainer = elementOffset.top - containerOffset.top +
    // Relative to the document element does not need to account for document scrollTop
    (container === 'page' ? 0 : containerScrollTop);
  if(mode === 'bottom' || mode === 'closest-if-needed' && elementOffsetInContainer + elementRect.height > containerScrollTop + containerRect.height - bottomMargin) {
    var newTop = elementOffsetInContainer - containerRect.height + elementRect.height + bottomMargin;
    scrollerElement.scrollTo({left:0, top: newTop, behavior:smooth ? 'smooth' : 'auto'});
  } else if (mode === 'top' || mode === 'closest-if-needed' && elementOffsetInContainer < containerScrollTop) {
    var newTop = elementOffsetInContainer - topMargin;
    scrollerElement.scrollTo({left:0, top: newTop, behavior: smooth ? 'smooth' : 'auto'});
  }
};


var defaultSlugifyConfig = {
  shorter: false,
  h0: false,
  h1: true,
  h2: true,
  h3: true,
  h4: true,
  h5: false,
  h6: false
};

var defaultSidenavifyConfig = {
  h1: true,
  h2: true,
  h3: true,
  h4: false,
  h5: false,
  h6: false
};

var defaultSlugContributions = {
  h1: true,
  h2: true,
  h3: true,
  h4: true,
  h5: true,
  h6: true
};

// Thank you David Walsh:
// https://davidwalsh.name/query-string-javascript
function queryParam(name) {
  var res = (
    new RegExp('[\\?&]' + (name.replace(/[\[]/, '\\[').replace(/[\]]/, '\\]'))+ '=([^&#]*)')
  ).exec(location.search);
  return res === null
    ? ''
    : decodeURIComponent(res[1].replace(/\+/g, ' '));
}

function parseYamlHeader(markdown, locationPathname) {
  if(markdown.indexOf('---\n') === 0) {
    var withoutFirstDashes = markdown.substr(4);
    var nextDashesIndex = withoutFirstDashes.indexOf('\n---\n');
    if(nextDashesIndex !== -1) {
      var potentialYamlContent = withoutFirstDashes.substr(0, nextDashesIndex);
      var lines = potentialYamlContent.split('\n');
      var props = {};
      for(var i = 0; i < lines.length; i++) {
        var colonIndex = lines[i].indexOf(':');
        if(colonIndex === -1) {
          return {markdown: markdown, headerProps: {}};
        } else {
          var field = lines[i].substr(0, colonIndex);
          // Todo: escape strings
          var content = lines[i].substr(colonIndex+1).trim();
          if(content[0] === '"' && content[content.length -1 ] === '"') {
            var strContent = content.substr(1, content.length -2);
            content = content.replace(new RegExp('\\\\"', 'g'), '"');
          }
          props[field] = content;
        }
      }
      if(!props.id) {
        var filename = locationPathname.substring(locationPathname.lastIndexOf('/') + 1);
        props.id = filename.indexOf('.') !== -1 ? filename.substring(0, filename.lastIndexOf('.')) : filename;
      }
      return {
        markdown: withoutFirstDashes.substr(nextDashesIndex + 4),
        headerProps: props
      };
    } else {
      return {markdown: markdown, headerProps: {}};
    }
  } else {
    return {markdown: markdown, headerProps: {}};
  }
}

/**
 * Strips out a special case of markdown "comments" which is supported in all
 * markdown parsers, will not be rendered in Github previews, but can be used
 * to convey yaml header information.
 *
 * Include this in your doc to have Bookmark interpret the yaml headers without
 * it appearing in the Github preview. This allows using one source of truth
 * markdown file for Github READMEs as well as using to generate your site
 * (when you don't want metadata showing up in your Github previews).
 *
 *     [//]: # (---)
 *     [//]: # (something: hey)
 *     [//]: # (title: me)
 *     [//]: # (description: "Hi there here is an escaped quote \" inside of quotes")
 *     [//]: # (---)
 */
function normalizeYamlMarkdownComments(markdown) {
  markdown = markdown.trim();
  if(markdown.indexOf('[//]: # (---)\n') === 0) {
    var withoutFirstDashes = markdown.substr(14);
    var nextDashesIndex = withoutFirstDashes.indexOf('\n[//]: # (---)\n');
    if(nextDashesIndex !== -1) {
      var potentialYamlContent = withoutFirstDashes.substr(0, nextDashesIndex);
      var lines = potentialYamlContent.split('\n');
      var yamlLines = ['---'];
      for(var i = 0; i < lines.length; i++) {
        var line = lines[i];
        var commentStartIndex = line.indexOf('[//]: # (');
        if(commentStartIndex !== 0 || line[line.length - 1] !== ')') {
          return markdown;
        } else {
          var commentContent = line.substr(9, line.length - 9 - 1); /*Minus one to trim last paren*/
          yamlLines.push(commentContent);
        }
      }
      yamlLines.push('---');
      return yamlLines.join('\n') + withoutFirstDashes.substr(nextDashesIndex + 15);
    } else {
      return markdown;
    }
  } else {
    return markdown;
  }
}

/**
 * The user can put this in their html file to:
 * 1. Get vim syntax highlighting to work.
 * 2. Get github to treat their html/htm file as a markdown file for rendering.
 * 3. Load the script tag only when rendered with ReFresh.
 *
 * [ vim:syntax=Markdown ]: # (<script src="flatdoc.js"></script>)
 *
 * Only downside is that it leaves a dangling ) in the text returned to
 * us which we can easily normalize.
 */
function normalizeMarkdownResponse(markdown) {
  if(markdown[0] === ')' && markdown[1] === '\n') {
    markdown = markdown.substring(2);
  }
  return markdown;
}

/**
 * [^] means don't match "no" characters - which is all characters including
 * newlines. The ? makes it not greddy.
 */
var docusaurusTabsRegionRegex = new RegExp(
  "^" + escapeRegExpSearchString("<!--DOCUSAURUS_CODE_TABS-->") +
  "$([^]*?)" +
  escapeRegExpSearchString("<!--END_DOCUSAURUS_CODE_TABS-->"),
  'gm'
);
var nonDocusaurusTabsRegionRegex = new RegExp(
  "^" + escapeRegExpSearchString("<!--CODE_TABS-->") +
  "$([^]*?)" +
  escapeRegExpSearchString("<!--END_CODE_TABS-->"),
  'gm'
);
var anyHtmlCommentRegex = new RegExp(
  "(^(" + escapeRegExpSearchString("<!--") +
    "([^]*?)" +
    escapeRegExpSearchString("-->") + ")[\n\r])?^```(.+)[\n\r]([^]*?)[\n\r]```",
  'gm'
);
function normalizeDocusaurusCodeTabs(markdown) {
  // Used to look it up later in the DOM and move things around to a more
  // convenient structure targetable by css.
  var onReplace = function(matchedStr, matchedCommentContents) {
    var tabs = [];
    var maxLengthOfCode = 0;
    var getMaxLengthOfCode = function(matchedStr, _, _, commentContent, syntax, codeContents) {
      var split = codeContents.split('\n');
      maxLengthOfCode =
        codeContents && split.length > maxLengthOfCode ? split.length : maxLengthOfCode;
      return matchedStr;
    };
    var onIndividualReplace = function(_, _, _, commentContent, syntax, codeContents) {
      var className = tabs.length === 0 ? 'active' : '';
      var split = codeContents.split('\n');
      var splitLen = split.length;
      // For some reason - 1 is needed when adding empty strings, instead of
      // non-empty spacers.
      while(splitLen - 1 < maxLengthOfCode) {
        split.push(" ");
        splitLen++;
      }
      tabs.push({
        syntax: syntax,
        codeContents: split.join("\n"),
        tabMarkup: "<codetabbutton class='" + className + "'" +
          " data-index=" + (tabs.length + 1) + ">" +
          escapeHtml(commentContent || syntax) + "</codetabbutton>"
      });
      return "\n```" + syntax + "\n" + split.join("\n") + "\n```";
    };
    tabs = [];
    maxLengthOfCode = 0;
    matchedCommentContents.replace(anyHtmlCommentRegex, getMaxLengthOfCode);
    var ret = matchedCommentContents.replace(anyHtmlCommentRegex, onIndividualReplace);
    return "<codetabscontainer data-num-codes=" +
      tabs.length +
      " class='bookmark-codetabs-active1 bookmark-codetabs-length" +
      tabs.length +
      "'>" +
      tabs.map(function(t){return t.tabMarkup;}).join("") +
      "</codetabscontainer>" +
      ret;
  };
  var ret = markdown.replace(docusaurusTabsRegionRegex, onReplace);
  return ret;
}

var emptyHTML = "";

/**
 * Scrolling into view:
 * https://www.bram.us/2020/03/01/prevent-content-from-being-hidden-underneath-a-fixed-header-by-using-scroll-margin-top/
 */

function escapePlatformStringLoop(html, lastIndex, index, s, len) {
  var html__0 = html;
  var lastIndex__0 = lastIndex;
  var index__0 = index;
  for (; ; ) {
    if (index__0 === len) {
      var match = 0 === lastIndex__0 ? 1 : 0;
      if (0 === match) {
        var match__0 = lastIndex__0 !== index__0 ? 1 : 0;
        return 0 === match__0 ?
          html__0 :
          html__0+s.substring(lastIndex__0, len);
      }
      return s;
    }
    var code = s.charCodeAt(index__0);
    if (40 <= code) {
      var switcher = code + -60 | 0;
      if (! (2 < switcher >>> 0)) {
        switch (switcher) {
          case 0:
            var html__1 = html__0+s.substring(lastIndex__0, index__0);
            var lastIndex__1 = index__0 + 1 | 0;
            var html__2 = html__1+"&lt;";
            var index__2 = index__0 + 1 | 0;
            var html__0 = html__2;
            var lastIndex__0 = lastIndex__1;
            var index__0 = index__2;
            continue;
          case 1:break;
          default:
            var html__3 = html__0+s.substring(lastIndex__0, index__0);
            var lastIndex__2 = index__0 + 1 | 0;
            var html__4 = html__3+"&gt;";
            var index__3 = index__0 + 1 | 0;
            var html__0 = html__4;
            var lastIndex__0 = lastIndex__2;
            var index__0 = index__3;
            continue
          }
      }
    }
    else if (34 <= code) {
      var switcher__0 = code + -34 | 0;
      switch (switcher__0) {
        case 0:
          var su = s.substring(lastIndex__0, index__0);
          var html__5 = html__0+su;
          var lastIndex__3 = index__0 + 1 | 0;
          var html__6 = html__5+"&quot;";
          var index__4 = index__0 + 1 | 0;
          var html__0 = html__6;
          var lastIndex__0 = lastIndex__3;
          var index__0 = index__4;
          continue;
        case 4:
          var su__0 = s.substring(lastIndex__0, index__0);
          var html__7 = html__0+su__0;
          var lastIndex__4 = index__0 + 1 | 0;
          var html__8 = html__7+"&amp;";
          var index__5 = index__0 + 1 | 0;
          var html__0 = html__8;
          var lastIndex__0 = lastIndex__4;
          var index__0 = index__5;
          continue;
        case 5:
          var su__1 = s.substring(lastIndex__0, index__0);
          var html__9 = html__0+su__1;
          var lastIndex__5 = index__0 + 1 | 0;
          var html__10 = html__9+"&#x27;";
          var index__6 = index__0 + 1 | 0;
          var html__0 = html__10;
          var lastIndex__0 = lastIndex__5;
          var index__0 = index__6;
          continue
        }
    }
    var index__1 = index__0 + 1 | 0;
    var index__0 = index__1;
    continue;
  }
}

function escapeHtml(s) {
  return (
    escapePlatformStringLoop(
      emptyHTML,
      0,
      0,
      s,
      s.length
    )
  );
}


var updateContextFromTreeNode = function(context, treeNode) {
  if(treeNode.level === 0) {
    return {...context, h0: treeNode, h1: null, h2: null, h3: null, h4: null, h5: null, h6: null};
  }
  if(treeNode.level === 1) {
    return {...context, h1: treeNode, h2: null, h3: null, h4: null, h5: null, h6: null};
  }
  if(treeNode.level === 2) {
    return {...context, h2: treeNode, h3: null, h4: null, h5: null, h6: null};
  }
  if(treeNode.level === 3) {
    return {...context, h3: treeNode, h4: null, h5: null, h6: null};
  }
  if(treeNode.level === 4) {
    return {...context, h4: treeNode, h5: null, h6: null};
  }
  if(treeNode.level === 5) {
    return {...context, h5: treeNode, h6: null};
  }
  if(treeNode.level === 6) {
    return {...context, h6: treeNode};
  }
  // LEAF_LEVEL
  return context;
};

/**
 * Turn a search string into a regex portion.
 * https://stackoverflow.com/a/1144788
 */
function escapeRegExpSearchString(string) {
  return string.replace(/[.*+\-?^${}()|[\]\\]/g, '\\$&');
}

function replaceAllStringsCaseInsensitive(str, find, replace) {
  return str.replace(new RegExp(escapeRegExp(find), 'gi'), replace);
}

function escapeRegExpSplitString(string) {
  return string.replace(/[.*+\-?^${}()|[\]\\]/g, '\\$&');
}

function splitStringCaseInsensitiveImpl(regexes, str, find) {
  return str.split(regexes.caseInsensitive.anywhere);
}
function splitStringCaseInsensitive(str, find) {
  return str.split(new RegExp('(' + escapeRegExpSplitString(find) + ')', 'gi'));
}

/**
 * Only trust for markdown that came from trusted source (your own page).
 * I do not know exactly what portions are unsafe - perhaps none.
 */
var trustedTraverseAndHighlightImpl = function traverseAndHighlightImpl(regex, text, node) {
  var tagName = node.nodeType === Node.TEXT_NODE ? 'p' : node.tagName.toLowerCase();
  var className = node.nodeType === Node.TEXT_NODE ? '' : node.getAttributeNode("class");
  var childNodes = node.nodeType === Node.TEXT_NODE ? [node] : node.childNodes;
  var childNode = childNodes.length > 0 ? childNodes[0] : null;
  var i = 0;
  var newInnerHtml = '';
  while(childNode && i < 2000) {
    if(childNode.nodeType === Node.TEXT_NODE) {
      if(regex) {
        var splitOnMatch = splitStringCaseInsensitiveImpl(regex, childNode.textContent, text);
        splitOnMatch.forEach(function(seg) {
          if(seg !== '') {
            if(seg.toLowerCase() === text.toLowerCase()) {
              newInnerHtml +=
                ('<search-highlight>' + escapeHtml(seg) + '</search-highlight>');
            } else {
              newInnerHtml += escapeHtml(seg);
            }
          }
        });
      } else {
        newInnerHtml += escapeHtml(childNode.textContent);
      }
    } else {
      newInnerHtml += trustedTraverseAndHighlightImpl(regex, text, childNode);
    }
    i++;
    childNode = childNodes[i];
  }
  var openTag = '';
  var closeTag = '';
  classAttr = className ? ' class="' + escapeHtml(className.value.replace('bookmark-in-doc-highlight', '')) + '"' : '';
  switch(tagName) {
    case 'a':
      var href = node.getAttributeNode("href");
      openTag = href ? '<a onclick="false" tabindex=-1 ' + classAttr + '>' : '<a>';
      closeTag = '</a>'
      break;
    case 'code':
      var className = node.getAttributeNode("class");
      openTag = className ? '<code class="' + escapeHtml(className.value) + '"' + classAttr + '>' : '<code>';
      closeTag = '</code>'
      break;
    default:
      openTag = '<' + tagName + classAttr + '>';
      closeTag = '</' + tagName + '>';
  }
  return openTag + newInnerHtml + closeTag;
}

var trustedTraverseAndHighlight = function(searchRegex, text, node) {
  return trustedTraverseAndHighlightImpl(searchRegex, text, node);
};


/**
 * Leaf nodes will be considered level 999 (something absurdly high).
 */
var LEAF_LEVEL = 999;
var PAGE_LEVEL = -1;
var getDomNodeStructureLevel = function getStructureLevel(node) {
  if(node.tagName === 'h0' || node.tagName === 'H0') {
    return 0;
  }
  if(node.tagName === 'h1' || node.tagName === 'H1') {
    return 1;
  }
  if(node.tagName === 'h2' || node.tagName === 'H2') {
    return 2;
  }
  if(node.tagName === 'h3' || node.tagName === 'H3') {
    return 3;
  }
  if(node.tagName === 'h4' || node.tagName === 'H4') {
    return 4;
  }
  if(node.tagName === 'h5' || node.tagName === 'H5') {
    return 5;
  }
  if(node.tagName === 'h6' || node.tagName === 'H6') {
    return 6;
  }
  return LEAF_LEVEL;
};
var deepensContext = function(treeNode) {
  return treeNode.level >= 0 && treeNode.level < 7;
};

/**
 * Searches up in the context for the correct place for this level to be
 * inserted.
 */
function recontext(context, nextTreeNode) {
  // Root document level is level zero.
  while(context.length > 1 && context[context.length - 1].level >= nextTreeNode.level) {
    context.pop();
  }
};

function hierarchicalIndexForSearch(pageState) {
  for(var pageKey in pageState) {
    if(!pageState[pageKey].hierarchicalIndex) {
      var containerNode = pageState[pageKey].contentContainerNode;
      pageState[pageKey].hierarchicalIndex = hierarchicalIndexFromHierarchicalDoc(pageState[pageKey].hierarchicalDoc);
    }
  }
}
function mapHierarchyOne(f, treeNode) {
  return  {
    levelContent: f(treeNode.levelContent),
    level: treeNode.level,
    subtreeNodes: mapHierarchy(f, treeNode.subtreeNodes)
  };
}
function mapHierarchy(f, treeNodes) {
  return treeNodes.map(mapHierarchyOne.bind(null, f));
}

function forEachHierarchyOne(f, context, treeNode) {
  var newContext = updateContextFromTreeNode(context, treeNode);
  f(treeNode.levelContent, treeNode.level, treeNode.subtreeNodes, newContext);
  forEachHierarchyImpl(f, newContext, treeNode.subtreeNodes);
}
function forEachHierarchyImpl(f, context, treeNodes) {
  return treeNodes.forEach(forEachHierarchyOne.bind(null, f, context));
}
function forEachHierarchy(f, treeNodes) {
  var context = startContext;
  return treeNodes.forEach(forEachHierarchyOne.bind(null, f, context));
}

/**
 * Returns a hierarchy tree where level contents are the individual items that
 * may be searchable. The original structured hierarchy tree has the
 * levelContent of each subtreeNode being the root node of every element that
 * appears directly under that heading. The hierarchicalIndex expands a single
 * tree node (such as one for a ul element) into several tree nodes (one for
 * each li in the ul for example). So it's a pretty simple mapping of the tree,
 * where each levelContent is expanded out into an array of content.
 */
function hierarchicalIndexFromHierarchicalDoc(treeNodes) {
  function expandTreeNodeContentToSearchables(domNode) {
    if(isNodeSearchHit(domNode)) {
      return [domNode];
    } else {
      var more = [];
      var childDomNode = domNode.firstChild;
      while(childDomNode) {
        more = more.concat(expandTreeNodeContentToSearchables(childDomNode));
        childDomNode = childDomNode.nextSibling;
      }
      return more;
    }
  };
  return treeNodes.map(mapHierarchyOne.bind(null, expandTreeNodeContentToSearchables));
}


/**
 * Forms a hierarchy of content from structure forming nodes (such as headers)
 * from what would otherwise be a flat document.
 * The subtreeNodes are not dom Subtree nodes but the hierarchy subtree (level
 * heading content etc).
 *
 * The subtreeNodes are either the list of Dom nodes immediately under that
 * level, else another "tree" node. (Type check it at runtime by looking for
 * .tagName property).
 *
 *  page:
 *
 *  H1Text
 *  text
 *  H2Text
 *  textB
 *  textC
 *
 *  Would be of the shape:
 *  {
 *    level: 0,                                                                // page
 *    levelContent: null,
 *    subtreeNodes: [
 *      {
 *        level: 1,
 *        levelContent: <h1>H1Text</h1>,                                       // h1 dom node
 *        subtreeNodes: [
 *          {level: LEAF_LEVEL, levelContent: <p>text</p>},                    // p DOM node
 *          {
 *            level: 2,
 *            levelContent: <h2>H2Text</h2>,
 *            subtreeNodes: [
 *              {level: LEAF_LEVEL, levelContent: <p>textB</p>},
 *              {level: LEAF_LEVEL, levelContent: <p>textC</p>}
 *            ]
 *          }
 *        ]
 *
 *      }
 *
 *    ]
 *  }
 */
function hierarchize(containerNode) {
  // Mutable reference.
  var dummyNode =  {
    // Such as the h2 node that forms the new level etc.
    levelContent: null,
    level: PAGE_LEVEL,
    subtreeNodes: []
  };
  var context = [dummyNode];
  function hierarchicalIndexChildrenImpl(domNode) {
    var childDomNode = domNode.firstChild;
    while(childDomNode) {
      hierarchicalIndexImpl(childDomNode);
      childDomNode = childDomNode.nextSibling;
    }
  };
  function hierarchicalIndexImpl(domNode) {
    var domNodeLevel = getDomNodeStructureLevel(domNode);
    var treeNode = {
      levelContent: domNode,
      level: domNodeLevel,
      subtreeNodes: []
    };
    recontext(context, treeNode);
    context[context.length - 1].subtreeNodes.push(treeNode);
    if(deepensContext(treeNode)) {
      context.push(treeNode)
    }
  };
  hierarchicalIndexChildrenImpl(containerNode);
  return dummyNode.subtreeNodes;
};

var filterHierarchicalSearchablesLowerCaseImpl = function(searchRegex, txt, pageState, renderTopRow) {
  var totalResultsLen = function() {
    return smartCaseWordBoundaryResults.length +
      smartCaseAnywhereNotWordBoundaryResults.length +
      caseInsensitiveWordBoundaryResults.length +
      caseInsensitiveAnywhereNotWordBoundaryResults.length;
  };
  var smartCaseWordBoundaryResults = [];
  var smartCaseAnywhereNotWordBoundaryResults = [];
  var caseInsensitiveWordBoundaryResults = [];
  var caseInsensitiveAnywhereNotWordBoundaryResults = [];
  // On the first keystroke, it will return far too many results, almost all of
  // them useless since it matches anything with that character. In that case, limit to
  // 20 results. Then on the next keystroke allow more.
  var maxResultsLen = txt.length === 1 ? 20 : 999;

  
  Flatdoc.forEachPage(pageState, (pageData, pageKey) => {
    forEachSearchableInHierarchy(function(searchable) {
      if(totalResultsLen() < maxResultsLen) {
        var node = searchable.node;
        var context = searchable.context;
        var nodeText = node.nodeType === Node.TEXT_NODE ? node.textContent : node.innerText;
        var test = findBestMatch(nodeText, searchRegex);
        var resultsToPush =
          test === -1 ? null :
          test & (SMARTCASE | WORDBOUNDARY) ? smartCaseWordBoundaryResults :
          test & (SMARTCASE) ? smartCaseAnywhereNotWordBoundaryResults :
          test & (WORDBOUNDARY) ? caseInsensitiveAnywhereNotWordBoundaryResults :
          caseInsensitiveAnywhereNotWordBoundaryResults;
        if(resultsToPush !== null) {
          // TODO: Show multiple matches per searchable.
          resultsToPush.push({
            searchable: searchable,
            highlightedInnerText: trustedTraverseAndHighlight(searchRegex, txt, node),
            topRow: renderTopRow(searchable.context, node)
          })
        }
      }
    }, pageData.hierarchicalIndex);
  });
  return smartCaseWordBoundaryResults.concat(
    smartCaseAnywhereNotWordBoundaryResults
  ).concat(
    caseInsensitiveWordBoundaryResults)
  .concat(
    caseInsensitiveAnywhereNotWordBoundaryResults
  )
};

/**
 * Invokes the callback for each searchable tree node (which has expanded array
 * of levelContent), will not invoke callback for subtree of that searchable
 * node, even if they are searchable.
 */
function forEachSearchableInHierarchyImpl(cb, treeNode, context) {
  for(var j = 0; j < treeNode.levelContent.length; j++) {
    var searchable = {node: treeNode.levelContent[j], context: context};
    cb(searchable);
  }
  context = updateContextFromTreeNode(context, treeNode);
  forEachSearchableInHierarchy(cb, treeNode.subtreeNodes, context);
}
function forEachSearchableInHierarchy(cb, treeNodes, context) {
  var context = typeof context === 'undefined' ? startContext : context;
  for(var i = 0; i < treeNodes.length; i++) {
    forEachSearchableInHierarchyImpl(cb, treeNodes[i], context);
  }
}

/**
 * Useful for converting unfiltered results into a "filtered" "highlighted" results set.
 */
var noopHierarchicalFilter = function(pageState, renderTopRow) {
  var results = [];
  // TODO: Here is where you would categorize the results by page.
  Flatdoc.forEachPage(pageState, (pageData, pageKey) => {
    forEachSearchableInHierarchy(function(searchable) {
      results.push({
        searchable: searchable,
        highlightedInnerText: trustedTraverseAndHighlight(null, "", searchable.node),
        topRow: renderTopRow(searchable.context, searchable.node)
      });
    }, pageData.hierarchicalIndex);
  });
  return results;
};

var SMARTCASE = 0b10;
var WORDBOUNDARY = 0b01;

var regexesFor = function(str) {
  var hasUpper = str.toLowerCase() !== str;
  return {
    // TODO: Add checks that remove symbols like hyphen, dot, parens
    smartCase: {
      // Priority 1
      wordBoundary: !hasUpper ? null : new RegExp('\\b(' + escapeRegExpSplitString(str) + ')', 'g' + (hasUpper ? '' : 'i')),
      // Priority 2
      anywhere:  !hasUpper ? null : new RegExp('(' + escapeRegExpSplitString(str) + ')', 'g' + (hasUpper ? '' : 'i'))
    },
    caseInsensitive: {
      // Priority 3
      wordBoundary: new RegExp('\\b(' + escapeRegExpSplitString(str) + ')', 'gi'),
      // Priority 4
      anywhere: new RegExp('(' + escapeRegExpSplitString(str) + ')', 'gi')
    }
  }
};

var findBestMatch = function(stringToTest, regexes) {
  if(regexes.smartCase.wordBoundary && regexes.smartCase.wordBoundary.test(stringToTest)) {
    return SMARTCASE | WORDBOUNDARY;
  } else if(regexes.smartCase.anywhere && regexes.smartCase.anywhere.test(stringToTest)) {
    return SMARTCASE;
  } else if(regexes.caseInsensitive.wordBoundary.test(stringToTest)) {
    return WORDBOUNDARY;
  } else if (regexes.caseInsensitive.anywhere.test(stringToTest)) {
    return 0
  } else {
    return -1;
  }
};

var filterHierarchicalSearchables = function(query, pageState, renderTopRow) {
  var txt = query.trim();
  var searchRegex = regexesFor(txt);new RegExp('(' + escapeRegExpSplitString(txt) + ')', 'gi');
  return filterHierarchicalSearchablesLowerCaseImpl(searchRegex, txt, pageState, renderTopRow);
};

var matchedSearchableToHit = function(matchedSearchable) {
  return {
    category:
      matchedSearchable.searchable.context.h3 ? matchedSearchable.searchable.context.h3.innerText :
      matchedSearchable.searchable.context.h2 ? matchedSearchable.searchable.context.h2.innerText :
      matchedSearchable.searchable.context.h1 ? matchedSearchable.searchable.context.h1.innerText : "",
    content: matchedSearchable.searchable.node.innerText,
    matchedSearchable: matchedSearchable
  }
};


/* Matches found in the header itself will be considered in that context */
var startContext = {
  h0: null,
  h1: null,
  h2: null,
  h3: null,
  h4: null,
  h5: null,
  h6: null
};

var contextToSlug = function(context, slugContributions) {
  var slug = '';
  if(context.h0 && slugContributions.h0) {
    slug += ' ' + Flatdoc.slugify(context.h0.levelContent.innerText);
  }
  if(context.h1 && slugContributions.h1) {
    slug += ' ' + Flatdoc.slugify(context.h1.levelContent.innerText);
  }
  if(context.h2 && slugContributions.h2) {
    slug += ' ' + Flatdoc.slugify(context.h2.levelContent.innerText);
  }
  if(context.h3 && slugContributions.h3) {
    slug += ' ' + Flatdoc.slugify(context.h3.levelContent.innerText);
  }
  if(context.h4 && slugContributions.h4) {
    slug += ' ' + Flatdoc.slugify(context.h4.levelContent.innerText);
  }
  if(context.h5 && slugContributions.h5) {
    slug += ' ' + Flatdoc.slugify(context.h5.levelContent.innerText);
  }
  if(context.h6 && slugContributions.h6) {
    slug += ' ' + Flatdoc.slugify(context.h6.levelContent.innerText);
  }
  return Flatdoc.slugify(slug.length > 0 ? slug.substring(1) : '');
};
  


/**
 * Bookmark is just a paired down version of Flatdoc with some additional
 * features, and many features removed.
 *
 * This version of flatdoc can run in three modes:
 *
 * Main entrypoint script include (when included from an index.html or
 * foo.html).
 *
 *     <script start src="pathto/Bookmark.js"></script>
 *
 * Included in a name.md.html markdown document or name.styl.html Stylus
 * document at the start of file
 *
 *     <script src="pathto/Bookmark.js"></script>
 *     # Rest of markdown here
 *     - regular markdown
 *     - regular markdown
 *
 * or:
 *
 *     <script src="pathto/Bookmark.js"></script>
 *     Rest of .styl document here:
 *
 * As a node script which will bundle your page into a single file assuming you've run npm install.
 */

/**
 * Since we use one simple script for everything, we need to detect how it's
 * being used. If not a node script, it could be included form the main html
 * page, or from a docs/stylus page. The main script tag in the main page will
 * be run at a point where there's no body in the document. For doc pages
 * (markdown/stylus) it will have a script tag at the top which implicitly
 * defines a body.
 */
function detectDocOrStyleIfNotNodeScript() {
  var hasParentFrame = window.parent !== window;
  var hasBody = document.body !== null;
  return hasParentFrame || hasBody;
};

/**
 * Assuming you are a doc or a style file (in an html extension), is this
 * trying to be loaded as an async doc/style content fetch from another HTML
 * page, or is this file attempting to be loaded as the main entrypoint (wihout
 * going through an index.html or something?) All requests for doc content go
 * through the Bookmark loader, and will ensure there is a query param
 * indicating this.
 */
function detectMode() {
  if(typeof process !== 'undefined') {
    return 'bookmarkNodeMode';
  }
  if(detectDocOrStyleIfNotNodeScript()) {
    var isHostPageQueryingContent = queryParam('bookmarkContentQuery');
    if (isHostPageQueryingContent) {
      return 'bookmarkContentQuery';
    } else {
      return 'bookmarkEntrypoint';
    }
  }
  // Either loading from a non template index.dev.html, or we are running a
  // template that includes the Bookmark.js script. We are loading from a
  // previous bookmarkEntrypoint flow, which has been turned into
  // bookmarkLoadFromHostPage after injecting the template.
  return 'bookmarkLoadFromHostPage';
  
};

var MODE = detectMode();

/**
 * Here's the order of events that occur when using the local file system at least:
 * 1. body DOMContentLoaded
 * 2. body onload event
 * 3. settimeout 0 handler.
 */
if(MODE === 'bookmarkNodeMode') {
  if(process.argv && process.argv.length > 2 && process.argv[2] === 'bundle') {
    var fs = require('fs');
    var path = require('path');
    var Inliner = require('inliner');


    var siteDir = __dirname;

    var pathToChrome =
      process.platform === 'win32' ?
      path.join(require('process').env['LOCALAPPDATA'], 'Google', 'Chrome', 'Application', 'chrome.exe') :
      '/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome';

    var cmd = pathToChrome + " " + path.join(siteDir, "..", "README.html") + ' --headless --dump-dom --virtual-time-budget=400';
    var rendered = require('child_process').execSync(cmd).toString();

    var renderedHtmlPath = path.join(siteDir, "..", 'index.rendered.html');
    var indexHtmlPath = path.join(siteDir, "..", 'index.html');
    fs.writeFileSync(renderedHtmlPath, rendered);

    console.log("INLINING PAGE: ", indexHtmlPath);

    var options = {
      /* Make sure you have this set to true to avoid flickering jumps */
      images: true,
      compressCSS: true,
      compressJS: true,
      // If true, will mess with hljs.
      collapseWhitespace: false,
      nosvg: true, // by default, DO compress SVG with SVGO
      skipAbsoluteUrls: false,
      preserveComments: false,
      iesafe: false
    };

    new Inliner(renderedHtmlPath, options, function (error, html) {
      // compressed and inlined HTML page
      // console.log(html);
      if(error) {
        console.error(e);
        process.exit(1);
      }
      fs.writeFileSync(indexHtmlPath, html)
      process.exit(0);
    });
  }
}

else if(MODE === 'bookmarkContentQuery') {
    // We are being asked about the document content from some host page (like an index.html that
    // manually calls out to docs).
    document.write('<plaintext style="display:none">');
    document.addEventListener("DOMContentLoaded", function() {
      var plaintexts = document.querySelectorAll('plaintext');
      if(plaintexts.length === 1) {
        window.parent.postMessage({
          messageType: 'docPageContent' ,
          iframeName: window.name,
          // innerHtml escapes markup in plaintext in Safari, but not Chrome.
          // innerText behaves correctly for both.
          content: plaintexts[0].innerText
        }, "*");
      } else {
        window.parent.postMessage({
          messageType: "docPageError",
          iframeName: window.name,
          error: "There isn't exactly one plaintext tag inside of " + window.name +
          ". Something went wrong and we didn't inject the plaintext tag."
        }, "*");
      }
    });
} else if(MODE === 'bookmarkEntrypoint') {
    // This is the a workflow where the md html page itself wants to be loadable without
    // needing to be included via some index.html. In this mode it can specify a page template
    // in its markdown header.
    
    // Remove the typical leading content before the script: This just helps
    // minimize the flash of that text. To completely eliminate it during
    // development mode, you can put this at the top of your md.
    // [ vim: set filetype=Markdown: ]: # (<style type="text/css">body {visibility: hidden} </style>)
    // while(document.body.hasChildNodes) {
    while(document.body.childNodes[0].nodeType === document.TEXT_NODE) {
      document.body.removeChild(document.body.childNodes[0]);
    }
    // Try to hide the plain text that comes before the important script include.
    // Minimize flash.
    document.write('<plaintext style="display:none">');
    // I find page reloads much less reliable if you document.close()
    // document.close();
    // However, I think this caused html contents inside of the markdown to be executed as html?
    window.onbeforeunload = function() {
    };
    document.addEventListener("DOMContentLoaded", function() {
      var plaintexts = document.querySelectorAll('plaintext');
      if(plaintexts.length === 1) {
        // innerHtml escapes markup in plaintext in Safari, but not Chrome.
        // innerText behaves correctly for both.
        // Parse out the yaml header just so we can get the siteTemplate, then
        // forward along the original markdown. Might as well leave the yaml
        // lines normalized.
        var markdown = normalizeMarkdownResponse(plaintexts[0].innerText);
        var markdownNormalizedYaml = normalizeYamlMarkdownComments(markdown);
        var markdownAndHeader = parseYamlHeader(markdownNormalizedYaml, window.location.pathname);
        if(typeof window.BookmarkTemplate === 'undefined') {
          window.BookmarkTemplate = {};
        }

        window.BookmarkTemplate.prefetchedCurrentPageBasename = urlBasename(window.location.href);
        window.BookmarkTemplate.prefetchedCurrentPageMarkdownAndHeader = markdownNormalizedYaml;
        // Set the variables for templates to read from.
        // https://www.geeksforgeeks.org/how-to-replace-the-entire-html-node-using-javascript/
        if(markdownAndHeader.headerProps.siteTemplate) {
          var templateFetchStart = Date.now();
          /**
           * The iframe's onDone will fire before the document's readystatechange 'complete' event.
           */
          var onDone = function(siteTemplate) {
            var templateFetchEnd = Date.now();
            console.log("fetching SITE TEMPLATE took", templateFetchEnd - templateFetchStart);
            var yetAnotherHtml = document.open("text/html", "replace"); 
            // If you want to listen for another readystatechange 'complete'
            // after images have loaded you have to create yetAnotherHtml This
            // isn't really needed since we don't listen to this.  Make sure to
            // hide the content while it is loading, since .write replaces.
            // flatdoc:ready will reveal it after images load.
            siteTemplate =
              siteTemplate.replace(
                new RegExp("(" +
                escapeRegExpSearchString( "<template>") +
                "|" + escapeRegExpSearchString( "</template>") +
                "|" + escapeRegExpSearchString( "<plaintext>") +
                ")", "g"),
                function(_) {return "";}
              );
            siteTemplate =
              siteTemplate.replace(
                new RegExp("\\$\\{Bookmark\\.Header\\.([^:\\}]*)}", 'g'),
                function(matchString, field) {
                  if(field !== 'siteTemplate' && field in markdownAndHeader.headerProps) {
                    return escapeHtml(markdownAndHeader.headerProps[field]);
                  }
                }
              );
            siteTemplate =
              siteTemplate.replace(
                new RegExp("\\$\\{Bookmark\\.Active\\.([^\\}]*)}", 'g'),
                function(matchString, field) {
                  return markdownAndHeader.headerProps.id === field ? 'active' : 'inactive';
                }
              );
            // The site template should also have
            //  <script>
            //    document.body.style="visibility:hidden"
            //  </script>
            //  So that when pre-rendered it is also correctly hidden
            yetAnotherHtml.write(siteTemplate); 
            yetAnotherHtml.close();
          };
          var onDoneCell = {contents: onDone};
          var onFailCell = {contents: (err) => {console.error(err);}};
          queryContentsViaIframe(markdownAndHeader.headerProps.siteTemplate, onDoneCell, onFailCell);
        } else {
          console.error(
            'You are loading a Bookmark doc from a markdown file, but that ' +
            'markdown doc does not specify a siteTemplate: in its yaml header.'
          );
        }
      } else {
        console.error(
          "There isn't exactly one plaintext tag inside of " + window.name +
          ". Something went wrong and we didn't inject the plaintext tag."
        );
      }
    });
  } else {
    // Must be 'bookmarkLoadFromHostPage' mode. At least populate this empty
    // dictionary so that when
    // BookmarkTemplate.prefetchedCurrentPageMarkdownAndHeader is accessed in
    // the rehydration workflow it doesn't fail (it will bail out) when it
    // realizes the page is already rendered though.
    if(typeof window.BookmarkTemplate === 'undefined') {
      window.BookmarkTemplate = {};
    }

(function($) {
  var exports = this;

  $.highlightNode = function(node) {
    $('.bookmark-in-doc-highlight').each(function() {
      var $el = $(this);
      $el.removeClass('bookmark-in-doc-highlight');
    });
    $(node).addClass('bookmark-in-doc-highlight');
  };


  var marked;

  /**
   * Basic Flatdoc module.
   *
   * The main entry point is Flatdoc.run(), which invokes the [Runner].
   *
   *     Flatdoc.run({
   *       fetcher: Flatdoc.github('rstacruz/backbone-patterns');
   *     });
   *
   * These fetcher functions are available:
   *
   *     Flatdoc.github('owner/repo')
   *     Flatdoc.github('owner/repo', 'API.md')
   *     Flatdoc.github('owner/repo', 'API.md', 'branch')
   *     Flatdoc.bitbucket('owner/repo')
   *     Flatdoc.bitbucket('owner/repo', 'API.md')
   *     Flatdoc.bitbucket('owner/repo', 'API.md', 'branch')
   *     Flatdoc.file('http://path/to/url')
   *     Flatdoc.file([ 'http://path/to/url', ... ])
   */

  var Flatdoc = exports.Flatdoc = {};
  exports.Bookmark = exports.Flatdoc;

  /**
   * Creates a runner.
   * See [Flatdoc].
   */
  Flatdoc.run = function(options) {
    var runner = new Flatdoc.runner(options)
    runner.run();
    return runner;
  };

  Flatdoc.mapPages = function(dict, onPage) {
    var result = {};
    for(var pageKey in dict) {
      result[pageKey] = onPage(dict[pageKey], pageKey);
    }
    return result;
  };
  Flatdoc.forEachPage = function(dict, onPage) {
    var _throwAway = Flatdoc.mapPages(
      dict,
      (pageData, pageKey) =>
        (onPage(pageData, pageKey), pageData)
    );
  };

  Flatdoc.keepOnly = function(dict, f) {
    var result = {};
    for(var pageKey in dict) {
      if(f(dict[pageKey]), pageKey) {
        result[pageKey] = dict[pageKey];
      }
    }
    return result;
  };

  Flatdoc.setFetcher = function(keyLowerCase, obj) {
    if(BookmarkTemplate.prefetchedCurrentPageBasename &&
      urlExtensionlessBasename(BookmarkTemplate.prefetchedCurrentPageBasename).toLowerCase() === keyLowerCase) {
      obj.fetcher = Bookmark.docPageContent(BookmarkTemplate.prefetchedCurrentPageMarkdownAndHeader);
    } else {
      obj.fetcher = Bookmark.docPage(keyLowerCase + ".html");
    }
  };
  /**
   * Simplified easy to use API that calls the underlying API.
   */
  Flatdoc.go = function(options) {
    var pageState = {};
    var actualOptions = {
      searchFormId: options.searchFormId,
      searchHitsId: options.searchHitsId,
      versionButtonId: options.versionButtonId,
      versionPageIs: options.versionPageIs ? options.versionPageIs.toLowerCase() : null,
      searchBreadcrumbContext: options.searchBreadcrumbContext,
      slugify: options.slugify || defaultSlugifyConfig,
      slugContributions: options.slugContributions || defaultSlugContributions,
      sidenavify: options.sidenavify || defaultSidenavifyConfig,
      pageState: pageState,
      effectiveHref: ''
    };
    if(options.stylus) {
      actualOptions.stylusFetcher = Flatdoc.docPage(options.stylus);
    }
    if(!options.pages) {
      alert("Error, no pages provided in Bookmark options");
      console.error("Error, no pages provided in Bookmark options");
    } else {
      var pages = options.pages;
      for(var pageKey in pages) {
        var pageKeyLowerCase = pageKey.toLowerCase();
        var page = pages[pageKey];
        pageState[pageKeyLowerCase] = {
          fetcher: null,
          markdownAndHeader: null,
          contentContainerNode: null,
          menuContainerNode: null,
          hierarchicalDoc: null,
          hierarchicalIndex: null
        };
        Flatdoc.setFetcher(pageKeyLowerCase, pageState[pageKeyLowerCase]);
      }
      if(options.highlight) {
        actualOptions.highlight = options.highlight;
      }
      var runner = Flatdoc.run(actualOptions);
    }
  };


  /**
   * File fetcher function.
   *
   * Fetches a given url via AJAX.
   * See [Runner#run()] for a description of fetcher functions.
   */

  Flatdoc.file = function(url) {
    function loadData(locations, response, callback) {
      if (locations.length === 0) callback(null, response);

      else $.get(locations.shift())
        .fail(function(e) {
          callback(e, null);
        })
        .done(function (data) {
          if (response.length > 0) response += '\n\n';
          response += data;
          loadData(locations, response, callback);
        });
    }

    return function(callback) {
      loadData(url instanceof Array ?
        url : [url], '', callback);
    };
  };

  /**
   * Runs with the already loaded string contents representing a doc.
   * This is used for "entrypoint mode".
   * TODO: Instead just maintain a cache, warm it up and use the regular
   * fetcher. This also allows reuse as a "style pre-fetch" property in the
   * yaml header.
   */
  Flatdoc.docPageContent = function(url) {
    if (!Flatdoc.errorHandler) {
      var listenerID = window.addEventListener('message', function(e) {
        if (e.data.messageType === 'docPageError') {
          console.error(e.data.error);
        }
      });
      Flatdoc.docPageErrorHandler = listenerID;
    }
    var fetchdocPage = function(content) {
      var onDone = null;
      var onFail = null;
      var returns = {
        fail: function(cb) {
          onFail = cb;
          return returns;
        },
        done: function(cb) {
          onDone = cb;
          onDone(content);
          return returns;
        }
      };
      return returns;
    };
    function loadData(locations, response, callback) {
      if (locations.length === 0) callback(null, response);
      else fetchdocPage(locations.shift())
        .fail(function(e) {
          callback(e, null);
        })
        .done(function (data) {
          if (response.length > 0) response += '\n\n';
          response += data;
          loadData(locations, response, callback);
        });
    }

    var url = url instanceof Array ? url : [url];
    var ret = function(callback) {
      loadData(url, '', callback);
    };
      // Tag the fetcher with the url in case you want it.
    ret.url = url;
    return ret;
  };
  
  /**
   * Local docPage doc fetcher function.
   *
   * Fetches a given url via iframe inclusion, expecting the file to be of
   * the "docPage" form of markdown which can be loaded offline.
   * See [Runner#run()] for a description of fetcher functions.
   *
   * Tags the url argument on the fetcher itself so it can be used for other
   * debugging/relativization.
   */

  Flatdoc.docPageErrorHandler = null;

  Flatdoc.docPage = function(url) {
    if (!Flatdoc.errorHandler) {
      var listenerID = window.addEventListener('message', function(e) {
        if (e.data.messageType === 'docPageError') {
          console.error(e.data.error);
        }
      });
      Flatdoc.docPageErrorHandler = listenerID;
    }
    var fetchdocPage = function(url) {
      var onDoneCell = {contents: null};
      var onFailCell = {contents: null};
      var returns = {
        fail: function(cb) {
          onFailCell.contents = cb;
          return returns;
        },
        done: function(cb) {
          onDoneCell.contents = cb;
          return returns;
        }
      };
      queryContentsViaIframe(url, onDoneCell, onFailCell);
      // Even if using the local file system, this will immediately resume
      // after appending without waiting or blocking.  There is no way to tell
      // that an iframe has loaded successfully without some kind of a timeout.
      // Even bad src locations will fire the onload event. An onerror event is
      // a solid signal that the page failed, but abscense of an onerror on the
      // iframe is not a confirmation of success or that it hasn't failed.
      return returns;
    };
    function loadData(locations, response, callback) {
      if (locations.length === 0) callback(null, response);
      else fetchdocPage(locations.shift())
        .fail(function(e) {
          callback(e, null);
        })
        .done(function (data) {
          if (response.length > 0) response += '\n\n';
          response += data;
          loadData(locations, response, callback);
        });
    }

    var url = url instanceof Array ? url : [url];
    var ret = function(callback) {
      loadData(url, '', callback);
    };
      // Tag the fetcher with the url in case you want it.
    ret.url = url;
    return ret;
  };

  /**
   * Github fetcher.
   * Fetches from repo repo (in format 'user/repo').
   *
   * If the parameter filepath` is supplied, it fetches the contents of that
   * given file in the repo's default branch. To fetch the contents of
   * `filepath` from a different branch, the parameter `ref` should be
   * supplied with the target branch name.
   *
   * See [Runner#run()] for a description of fetcher functions.
   *
   * See: http://developer.github.com/v3/repos/contents/
   */
  Flatdoc.github = function(opts) {
    if (typeof opts === 'string') {
      opts = {
        repo: arguments[0],
        filepath: arguments[1]
      };
    }
    var url;
    if (opts.filepath) {
      url = 'https://api.github.com/repos/'+opts.repo+'/contents/'+opts.filepath;
    } else {
      url = 'https://api.github.com/repos/'+opts.repo+'/readme';
    }
    var data = {};
    if (opts.token) {
      data.access_token = opts.token;
    }
    if (opts.ref) {
      data.ref = opts.ref;
    }
    return function(callback) {
      $.get(url, data)
        .fail(function(e) { callback(e, null); })
        .done(function(data) {
          var markdown = exports.Base64.decode(data.content);
          callback(null, markdown);
        });
    };
  };

  /**
   * Bitbucket fetcher.
   * Fetches from repo `repo` (in format 'user/repo').
   *
   * If the parameter `filepath` is supplied, it fetches the contents of that
   * given file in the repo.
   *
   * See [Runner#run()] for a description of fetcher functions.
   *
   * See: https://confluence.atlassian.com/display/BITBUCKET/src+Resources#srcResources-GETrawcontentofanindividualfile
   * See: http://ben.onfabrik.com/posts/embed-bitbucket-source-code-on-your-website
   * Bitbucket appears to have stricter restrictions on
   * Access-Control-Allow-Origin, and so the method here is a bit
   * more complicated than for Github
   *
   * If you don't pass a branch name, then 'default' for Hg repos is assumed
   * For git, you should pass 'master'. In both cases, you should also be able
   * to pass in a revision number here -- in Mercurial, this also includes
   * things like 'tip' or the repo-local integer revision number
   * Default to Mercurial because Git users historically tend to use GitHub
   */
  Flatdoc.bitbucket = function(opts) {
    if (typeof opts === 'string') {
      opts = {
        repo: arguments[0],
        filepath: arguments[1],
        branch: arguments[2]
      };
    }
    if (!opts.filepath) opts.filepath = 'readme.md';
    if (!opts.branch) opts.branch = 'default';

    var url = 'https://bitbucket.org/api/1.0/repositories/'+opts.repo+'/src/'+opts.branch+'/'+opts.filepath;

   return function(callback) {
     $.ajax({
      url: url,
      dataType: 'jsonp',
      error: function(xhr, status, error) {
        alert(error);
      },
      success: function(response) {
        var markdown = response.data;
        callback(null, markdown);
      }
  });
};
};

  /**
   * Parser module.
   * Parses a given Markdown document and returns a JSON object with data
   * on the Markdown document.
   *
   *     var data = Flatdoc.parser.parse('markdown source here');
   *     console.log(data);
   *
   *     data == {
   *       title: 'My Project',
   *       content: '<p>This project is a...',
   *       menu: {...}
   *     }
   */

  var Parser = Flatdoc.parser = {};

  /**
   * Parses a given Markdown document.
   * See `Parser` for more info.
   */
  Parser.parse = function(doc, markdownAndHeader, highlight, pageState, pageKey) {
    marked = exports.marked;

    Parser.setMarkedOptions(highlight);

    var html = $("<div>" + marked(markdownAndHeader.markdown));
    var title = markdownAndHeader.headerProps.title;
    if(!title) {
      title = html.find('h1').eq(0).text();
    }

    // Mangle content
    Transformer.mangle(doc, html, pageState, pageKey);
    var menu = Transformer.getMenu(doc, html);

    return {content: html, menu: menu};
  };

  Parser.setMarkedOptions = function(highlight) {
    marked.setOptions({
      highlight: function(code, lang) {
        if (lang) {
          return highlight(code, lang);
        }
        return code;
      }
    });

    marked.Renderer.prototype.paragraph = (text) => {
      if (text.startsWith("<codetabscontainer")) {
        return text + "\n";
      }
      return "<p>" + text + "</p>";
    };
  };

  /**
   * Transformer module.
   * This takes care of any HTML mangling needed.  The main entry point is
   * `.mangle()` which applies all transformations needed.
   *
   *     var $content = $("<p>Hello there, this is a docu...");
   *     Flatdoc.transformer.mangle($content);
   *
   * If you would like to change any of the transformations, decorate any of
   * the functions in `Flatdoc.transformer`.
   */

  var Transformer = Flatdoc.transformer = {};

  /**
   * Takes a given HTML `$content` and improves the markup of it by executing
   * the transformations.
   *
   * > See: [Transformer](#transformer)
   */
  Transformer.mangle = function(runner, pageKey, hierarchicalDoc) {
  };

  /**
   * Adds IDs to headings. What's nice about this approach is that it is
   * agnostic to how the markup is rendered.
   * TODO: These (better) links won't always work in markdown on github because
   * github doesn't encode subsections into the links. To address this, we can allow
   * Github links in the markdown and then transform them into the better ones
   * on the rendered page.  This produces more stable linked slugs.
   */
  Transformer.addIDsToHierarchicalDoc = function(runner, hierarchicalDoc, pageKey) {
    var seenSlugs = {};
    // Requesting side-nav requires linkifying
    var headers = 'h0 h1 h2 h3 h4 h5 h6 H0 H1 H2 H3 H4 H5 H6';
    forEachHierarchy(function(levelContent, level, subtreeNodes, inclusiveContext) {
      if(headers.indexOf(levelContent.tagName) !== -1) {
        var slugCandidate = contextToSlug(inclusiveContext, runner.slugContributions);
        var slug = seenSlugs[slugCandidate] ? (slugCandidate + '--' + (seenSlugs[slugCandidate] + 1)) : slugCandidate;
        seenSlugs[slugCandidate] = seenSlugs[slugCandidate] ? seenSlugs[slugCandidate] + 1 : 1;
        levelContent.id = linkifidIdForHash(pageifiedIdForHash(slug, pageKey));
      }
    }, hierarchicalDoc);
  };

  /**
   * Returns menu data for a given HTML.
   *
   *     menu = Flatdoc.transformer.getMenu($content);
   *     menu == {
   *       level: 0,
   *       items: [{
   *         section: "Getting started",
   *         level: 1,
   *         items: [...]}, ...]}
   */

  Transformer.getMenu = function(runner, $content) {
    var root = {items: [], id: '', level: 0};
    var cache = [root];

    function mkdir_p(level) {
      cache.length = level + 1;
      var obj = cache[level];
      if (!obj) {
        var parent = (level > 1) ? mkdir_p(level-1) : root;
        obj = { items: [], level: level };
        cache = cache.concat([obj, obj]);
        parent.items.push(obj);
      }
      return obj;
    }

    var query = [];
    if(runner.sidenavify.h1) {
      query.push('h1');
    }
    if(runner.sidenavify.h2) {
      query.push('h2');
    }
    if(runner.sidenavify.h3) {
      query.push('h3');
    }
    if(runner.sidenavify.h4) {
      query.push('h4');
    }
    if(runner.sidenavify.h5) {
      query.push('h5');
    }
    if(runner.sidenavify.h6) {
      query.push('h6');
    }
    $content.find(query.join(',')).each(function() {
      var $el = $(this);
      var level = +(this.nodeName.substr(1));

      var parent = mkdir_p(level-1);
      var text = $el.text();
      var el = $el[0];
      if(el.childNodes.length === 1 && el.childNodes[0].tagName === 'code' || el.childNodes[0].tagName === 'CODE') {
        text = '<code>' + text + '</code>';
      }
      var obj = { section: text, items: [], level: level, id: $el.attr('id') };
      parent.items.push(obj);
      cache[level] = obj;
    });

    return root;
  };

  /**
   * Changes "button >" text to buttons.
   */

  Transformer.buttonize = function(content) {
    $(content).find('a').each(function() {
      var $a = $(this);

      var m = $a.text().match(/^(.*) >$/);
      if (m) $a.text(m[1]).addClass('button');
    });
  };

  /**
   * Applies smart quotes to a given element.
   * It leaves `code` and `pre` blocks alone.
   */

  Transformer.smartquotes = function (content) {
    var nodes = getTextNodesIn($(content)), len = nodes.length;
    for (var i=0; i<len; i++) {
      var node = nodes[i];
      node.nodeValue = quotify(node.nodeValue);
    }
  };

  /**
   * Syntax highlighters.
   *
   * You may add or change more highlighters via the `Flatdoc.highlighters`
   * object.
   *
   *     Flatdoc.highlighters.js = function(code) {
   *     };
   *
   * Each of these functions
   */

  var Highlighters = Flatdoc.highlighters = {};

  /**
   * JavaScript syntax highlighter.
   *
   * Thanks @visionmedia!
   */

  Highlighters.js = Highlighters.javascript = function(code) {
    return code
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/("[^\"]*?")/g, '<span class="string">$1</span>')
      .replace(/('[^\']*?')/g, '<span class="string">$1</span>')
      .replace(/\/\/(.*)/gm, '<span class="comment">//$1</span>')
      .replace(/\/\*(.*)\*\//gm, '<span class="comment">/*$1*/</span>')
      .replace(/(\d+\.\d+)/gm, '<span class="number">$1</span>')
      .replace(/(\d+)/gm, '<span class="number">$1</span>')
      .replace(/\bnew *(\w+)/gm, '<span class="keyword">new</span> <span class="init">$1</span>')
      .replace(/\b(function|new|throw|return|var|if|else)\b/gm, '<span class="keyword">$1</span>');
  };

  Highlighters.html = function(code) {
    return code
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/("[^\"]*?")/g, '<span class="string">$1</span>')
      .replace(/('[^\']*?')/g, '<span class="string">$1</span>')
      .replace(/&lt;!--(.*)--&gt;/g, '<span class="comment">&lt;!--$1--&gt;</span>')
      .replace(/&lt;([^!][^\s&]*)/g, '&lt;<span class="keyword">$1</span>');
  };

  Highlighters.generic = function(code) {
    return code
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/("[^\"]*?")/g, '<span class="string">$1</span>')
      .replace(/('[^\']*?')/g, '<span class="string">$1</span>')
      .replace(/(\/\/|#)(.*)/gm, '<span class="comment">$1$2</span>')
      .replace(/(\d+\.\d+)/gm, '<span class="number">$1</span>')
      .replace(/(\d+)/gm, '<span class="number">$1</span>');
  };

  /**
   * Menu view. Renders menus
   */

  var MenuView = Flatdoc.menuView = function(menu) {
    var $el = $("<ul>");

    function process(node, $parent) {
      var id = node.id || 'root';
      var nodeHashToChangeTo = hashForLinkifiedId(id);

      var $li = $('<li>')
        .attr('id', id + '-item')
        .addClass('level-' + node.level)
        .appendTo($parent);

      if (node.section) {
        var $a = $('<a>')
          .html(node.section)
          .attr('id', id + '-link')
          .attr('href', '#' + nodeHashToChangeTo)
          .addClass('level-' + node.level)
          .appendTo($li);
        $a.on('click', function() {
          var foundNode = $('#' + node.id);
          foundNode && $.highlightNode(foundNode);
        });
      }

      if (node.items.length > 0) {
        var $ul = $('<ul>')
          .addClass('level-' + (node.level+1))
          .attr('id', id + '-list')
          .appendTo($li);

        node.items.forEach(function(item) {
          process(item, $ul);
        });
      }
    }

    process(menu, $el);
    return $el;
  };

  /**
   * A runner module that fetches via a `fetcher` function.
   *
   *     var runner = new Flatdoc.runner({
   *       fetcher: Flatdoc.url('readme.txt')
   *     });
   *     runner.run();
   *
   * The following options are available:
   *
   *  - `fetcher` - a function that takes a callback as an argument and
   *    executes that callback when data is returned.
   *
   * See: [Flatdoc.run()]
   */

  var Runner = Flatdoc.runner = function(options) {
    this.initialize(options);
  };

  Runner.prototype.pageRootSelector    = 'body';

  /**
   * Really, is used to model internal *component* state based on entered
   * control value.  Like if a text input is empty, the text input component
   * sets the search component to QueryStates.NONE_AND_HIDE.
   * If the user hits enter on a dropdown selector, it toggles it between NONE
   * and ALL.
   *
   * There's three bits of information per control that determine visibility:
   *
   * 1. Which component is "active" (like focused). This is currently modeled
   * by activeSearchComponent (but that is almost redundant with document focus). It's not
   * exactly the same as focused DOM element because we also want a component
   * to be able to keep the popup open even if the user tabs to other parts of
   * the document. That doesn't always make sense for every kind of component,
   * but it's a feature. So activeSearchComponent recreates _another_ notion of active
   * element apart from the document's.
   * 2. Whether or not the internal state of the component warrants showing any
   * popup. (QueryStates). Like a search input could have empty text which
   * warrants showing no results. Or a dropdown component (which always has
   * "empty text"), could be focused but it's not supposed to show any results
   * until you click or press enter/space. That internal component state helps
   * determine whether or not a popup should be shown. In the case of text
   * input this is redundant or derivable from its input text (but not the case
   * for other component types).
   * 3. Whether or not the user requested that a popup for the currently active
   * component be supressed. Even if 1 and 2 would otherwise result in showing
   * a popup, the user could press escape.
   * An autocomplete text input with non-empty input, that is currently focused
   * (or "active") could press ctrl-c closing the popup window.
   * A dropdown component could be "active", could have been clicked on, but
   * the user could click a second time closing it (or pressing escape).
   */
  var QueryStates = {
    NONE: 'NONE',
    ALL: 'ALL',
    FILTER: 'FILTER'
  };

  function SearchComponentBase(root, pages) {
    this.root = root;
    this.pages = pages;
    this.queryState = QueryStates.ALL;
    this.results = [];
    /**
     * This state is managed both internally and externally. Internally,
     * components know when they need to reset the user requested cursor. But
     * externally search lists know when to reach out and mutate this.
     */
    this.userRequestedCursor = null;
  };

  SearchComponentBase.effectiveCursorPosition = function() {
    return this.userRequestedCursor === null
      ? 0
      : this.userRequestedCursor;
  }

  function TextDocSearch(props) {
    SearchComponentBase.call(this, props.root, props.pages);
    this.queryState = QueryStates.FILTER;
    var placeholder = this.getPlaceholder();
    if(this.root.tagName.toUpperCase() !== 'FORM') {
      console.error('You provided a searchFormId that does not exist');
      return;
    }
    var theSearchInput;
    var theSearchClear;
    if(this.root.className.indexOf('bookmark-search-form-already-setup') !== -1) {
      theSearchInput = this.root.childNodes[0];
      theSearchClear = this.root.childNodes[1];
    } else {
      this.root.className += ' bookmark-search-form  bookmark-search-form-already-setup';
      this.root.onsubmit="";
      var theSearchInput = document.createElement('input');
      theSearchInput.name = 'focus';
      theSearchInput.className = 'bookmark-search-input';
      theSearchInput.placeholder = placeholder;
      theSearchInput.required = true;
      theSearchClear = document.createElement('button');
      theSearchClear.tabindex=1;
      theSearchClear.className='bookmark-search-input-right-reset-icon';
      theSearchClear.type='reset';
      theSearchClear.tabIndex=-1;
      this.root.prepend(theSearchClear);
      this.root.prepend(theSearchInput);
    }
    this.theSearchInput = theSearchInput;
    theSearchInput.addEventListener('focus', function(e) {
      var focusedPlaceholder = this.getFocusedPlaceholder(this.root);
      theSearchInput.placeholder = focusedPlaceholder;
      if(this.userRequestedCursor === -1) {
        this.userRequestedCursor = null;
      }
      if(this.valueWarrantsHiding()) {
        props.onDoesntWantActiveStatus && props.onDoesntWantActiveStatus(this);
      } else {
        props.onWantsToHaveActiveStatus && props.onWantsToHaveActiveStatus(this);
      }
      props.onFocus && props.onFocus(e);
    }.bind(this));
    theSearchInput.addEventListener('keydown', function(e) {
      props.onKeydown && props.onKeydown(e);
    }.bind(this));
    theSearchInput.addEventListener('input', function(e) {
      this.userRequestedCursor = null;
      if(this.valueWarrantsHiding()) {
        props.onDoesntWantActiveStatus && props.onDoesntWantActiveStatus(this);
      } else {
        props.onWantsToHaveActiveStatus && props.onWantsToHaveActiveStatus(this);
      }
      props.onInput && props.onInput(e);
    }.bind(this));
    theSearchInput.addEventListener('blur', function(e) {
      var focusedPlaceholder = this.getPlaceholder();
      theSearchInput.placeholder = focusedPlaceholder;
      props.onBlur && props.onBlur(e);
    }.bind(this));
    
    // This one goes on the form itself
    this.root.addEventListener('reset', function() {
      this.setValue('');
      this.focus();
      this.userRequestedCursor = null;
      props.onDoesntWantActiveStatus && props.onDoesntWantActiveStatus(this);
      if(props.onReset) {
        props.onReset
      }
    }.bind(this));
    this.root.addEventListener('submit', function(e) {
      e.preventDefault();
    }.bind(this));
  };
  TextDocSearch.prototype.getQuery = function() {
    return this.getValue().trim();
  };
  TextDocSearch.prototype.valueWarrantsHiding = function() {
    return this.getValue().trim() === '';
  };

  TextDocSearch.prototype.effectiveCursorPosition = SearchComponentBase.effectiveCursorPosition;

  TextDocSearch.prototype.getFocusedPlaceholder = function() {
    var defaultTxt = "Search (Esc close)";
    return this.root ?
      (this.root.dataset.focusedPlaceholder || defaultTxt) :
      defaultTxt;
  };
  TextDocSearch.prototype.getPlaceholder = function(root) {
    var defaultTxt = "Press '/' to focus" ;
    return this.root ?
      (this.root.dataset.placeholder || defaultTxt) :
      defaultTxt;
  };
  TextDocSearch.prototype.focus = function() {
    return this.theSearchInput.focus();
  };
  TextDocSearch.prototype.selectAll = function() {
    return this.theSearchInput.select();
  };
  TextDocSearch.prototype.isFocused = function() {
    return document.activeElement === this.theSearchInput;
  };
  TextDocSearch.prototype.blur = function() {
    return this.theSearchInput.blur();
  };
  TextDocSearch.prototype.getValue = function() {
    return this.theSearchInput.value;
  };
  TextDocSearch.prototype.setValue = function(v) {
    this.theSearchInput.value = v;
  };
  TextDocSearch.prototype.setPlaceholder = function(ph) {
    this.theSearchInput.placeholder = ph;
  };
  TextDocSearch.prototype.isSearchable = isNodeSearchHit;
  TextDocSearch.prototype.onLostActiveSearchComponent = function() {
    // this.queryState = QueryStates.NONE_AND_HIDE; 
  };
  TextDocSearch.prototype.onGainedActiveSearchComponent = function() {
    // this.queryState = QueryStates.ALL; 
  };

  /**
   * An "input selector" style component that uses the navigation autocomplete window.
   */
  function TextDocSelector(props) {
    SearchComponentBase.call(this, props.root, props.pages);
    this.queryState = QueryStates.ALL; 
    
    this.root.addEventListener('focus', function(e) {
      if(this.userRequestedCursor === -1) {
        this.userRequestedCursor = null;
      }
      props.onFocus && props.onFocus(e);
    });
    this.root.addEventListener('keydown', function(e) {
      props.onKeydown && props.onKeydown(e);
    });
    this.root.addEventListener('click', function(e) {
      if(props.isActiveComponent()) {
        props.onDoesntWantActiveStatus && props.onDoesntWantActiveStatus(this);
      } else {
        props.onWantsToHaveActiveStatus && props.onWantsToHaveActiveStatus(this);
      }
    }.bind(this));
    this.root.addEventListener('blur', function() {
      props.onDoesntWantActiveStatus && props.onDoesntWantActiveStatus(this);
    }.bind(this));
  };
  TextDocSelector.prototype.isSearchable = isNodeSearchHit;
  TextDocSelector.prototype.getQuery = function() {
    return '';
  };
  TextDocSelector.prototype.onLostActiveSearchComponent = function() {
  };
  TextDocSelector.prototype.onGainedActiveSearchComponent = function() {
  };

  TextDocSelector.prototype.effectiveCursorPosition = SearchComponentBase.effectiveCursorPosition;


  /**
   * Custom methods (extends base API for search components).
   */
  
  
  Runner.prototype.initialize = function(options) {
    this.pageState = {};
    this.searchState = {
      /**
       * "global" state - across all searches.
       */
      activeSearchComponent: null,
      /**
       * Typically until the next event that switches the active component.
       */
      userRequestedCloseEvenIfActive: true,
      VERSIONS: null,
      CONTENT: null
    }
    this.nodes = {
      theSearchHits: null,
      theHitsScrollContainer: null,
      versionMenuButton: null,
      versionsContainer: null
    }
    $.extend(this, options);
  };

  /**
   * Syntax highlighting.
   *
   * You may define a custom highlight function such as `highlight` from
   * the highlight.js library.
   *
   *     Flatdoc.run({
   *       highlight: function (code, value) {
   *         return hljs.highlight(lang, code).value;
   *       },
   *       ...
   *     });
   *
   */

  /**
   * There is only one active search component. It is the one that will be
   * responsible for providing search results. The moment a different component
   * becomes the new active component, the new active component determines
   * which results will be shown, and helps decide whether or not to show the
   * popup menu at all.
   */
  Runner.prototype.setActiveSearchComponent = function(newComp) {
    if(newComp !== this.searchState.activeSearchComponent) {
      if(this.searchState.activeSearchComponent) {
        this.searchState.activeSearchComponent.onLostActiveSearchComponent();
      }
      this.searchState.activeSearchComponent = newComp;
      this.searchState.userRequestedCloseEvenIfActive = false;
      if(this.searchState.activeSearchComponent) {
        this.searchState.activeSearchComponent.onGainedActiveSearchComponent();
      }
    }
  };
  Runner.prototype.highlight = function(code, lang) {
    var fn = Flatdoc.highlighters[lang] || Flatdoc.highlighters.generic;
    return fn(code);
  };

  Runner.prototype.noResultsNode = function(query) {
    var d = document.createElement('div');
    d.className = "bookmark-hits-noresults-list";
    d.innerText = 'No results for "' + query + '"';
    return d;
  };
  Runner.prototype.getHitsScrollContainer = function() {
    return this.nodes.theHitsScrollContainer;
  };
  Runner.prototype.effectiveCursorPosition = function(searchComponent) {
    return searchComponent.userRequestedCursor === null
      ? 0
      : searchComponent.userRequestedCursor;
  }
  Runner.prototype.updateSearchResultsList = function(searchComponent, query, results, clickHandler) {
    var doc = this;
    // var isNowVisible = doc.updateSearchHitsVisibility(searchComponent);
    // // If toggling to invisible, do not change the rendered list. The reason is
    // // that we want existing contents to always fade out instead of changing the list contents
    // // while fading out.
    // if(!isNowVisible) {
    //   return;
    // }
    var hitsScrollContainer = this.getHitsScrollContainer();
    var firstItem = null;
    var lastItem = null;
    var effectiveCursorPosition = this.effectiveCursorPosition(searchComponent);
    if(!results.length) {
      var len = hitsScrollContainer.childNodes.length;
      for(var i = 0; i < len; i++) {
        hitsScrollContainer.removeChild(hitsScrollContainer.childNodes[i]);
      }
      hitsScrollContainer.appendChild(this.noResultsNode(query));
    } else {
      var existingHitsList;
      var hitsList;
      if(hitsScrollContainer.childNodes[0] && hitsScrollContainer.childNodes[0].className === 'bookmark-hits-noresults-list') {
        existingHitsList = null;
        hitsScrollContainer.removeChild(hitsScrollContainer.childNodes[0]);
      } else {
        existingHitsList = hitsScrollContainer.childNodes[0];
      }
      if(!existingHitsList) {
        hitsList = document.createElement('div');
        hitsList.className = 'bookmark-hits-list';
        hitsScrollContainer.appendChild(hitsList);
      } else {
        hitsList = existingHitsList;
      }
      var numExistingHitsListItems = existingHitsList ? existingHitsList.childNodes.length : 0;
      for(var i = results.length; i < numExistingHitsListItems; i++) {
        existingHitsList.removeChild(existingHitsList.childNodes[existingHitsList.childNodes.length - 1]);
      }
      for(var i = 0; i < results.length; i++) {
        var category = results[i].category;
        var textContent = results[i].content;
        var _highlightResultContentValue = results[i].matchedSearchable.highlightedInnerText;
        var topRow = results[i].matchedSearchable.topRow;
        var hitsItem;
        var cursor = null;
        // Reuse dom nodes to avoid flickering of css classes/animation.
        if(existingHitsList && existingHitsList.childNodes[i]) {
          hitsItem =  existingHitsList.childNodes[i];
          $(hitsItem).off('click');
          hitsItem.removeChild(hitsItem.childNodes[0]);
        } else {
          hitsItem = document.createElement('div');
          hitsItem.tabIndex = -1;
          hitsItem.className = 'bookmark-hits-item';
          hitsList.appendChild(hitsItem);
        }
        if(effectiveCursorPosition === i) {
          cursor = $(hitsItem)[0];
          $(hitsItem).addClass('cursor');
        } else {
          $(hitsItem).removeClass('cursor');
        }
        $(hitsItem).on('click', function(i, e) {
          clickHandler(searchComponent, query, results, i, e);
        }.bind(null, i));
        var buttonContents = document.createElement('div');
        buttonContents.className='bookmark-hits-item-button-contents';
        buttonContents.innerHTML = _highlightResultContentValue;
        topRow && buttonContents.insertBefore(topRow, buttonContents.firstChild);
        hitsItem.appendChild(buttonContents);
        if(cursor) {
          customScrollIntoView({
            smooth: true,
            container: hitsScrollContainer,
            element: cursor,
            mode: 'closest-if-needed',
            topMargin: 10,
            bottomMargin: 10
          });
        }
      }
    }
  };
  Runner.prototype._createTopRow = function(row, context, level) {
    row = document.createElement('div');
    row.className = 'bookmark-hits-item-button-contents-top-row';
    var crumb = document.createElement('div');
    crumb.className = 'bookmark-hits-item-contents-top-row-crumb';
    var tags = document.createElement('div');
    tags.className = 'bookmark-hits-item-contents-top-row-tags';
    row.appendChild(crumb);
    row.appendChild(tags);
    return row;
  };
  Runner.prototype._appendContextCrumb = function(row, context, level) {
    if(!row) {
      row = this._createTopRow();
    }
    if(context[level]) {
      if(row.childNodes[0].childNodes.length > 0) {
        var chevron = document.createElement('span');
        chevron.className = 'bookmark-hits-item-button-contents-crumb-sep';
        chevron.innerText = '›';
        // Append to the crumb child
        row.childNodes[0].appendChild(chevron);
      }
      var seg = document.createElement('span');
      seg.className = 'bookmark-hits-item-button-contents-crumb-row-first';
      seg.innerText = context[level].levelContent[0].innerText;
      row.childNodes[0].appendChild(seg);
    }
    return row;
  };
  Runner.prototype.topRowForDocSearch = function(context, node) {
    var searchBreadcrumbContext =
      this.searchBreadcrumbContext ? this.searchBreadcrumbContext :
      defaultSearchBreadcrumbContext;
    var context = searchBreadcrumbContext(context);
    var row = this._appendContextCrumb(null, context, 'h1');
    row = this._appendContextCrumb(row, context, 'h2');
    row = this._appendContextCrumb(row, context, 'h3');
    row = this._appendContextCrumb(row, context, 'h4');
    row = this._appendContextCrumb(row, context, 'h5');
    row = this._appendContextCrumb(row, context, 'h6');
    return row;
  };
  Runner.prototype.setupHitsScrollContainer = function() {
    var theSearchHitsId = this.searchHitsId;
    var theSearchHits = document.getElementById(theSearchHitsId);
    var hitsScrollContainer = theSearchHits.childNodes[0];
    var hitsScrollContainerAppearsSetup =
      hitsScrollContainer &&
      hitsScrollContainer.className.indexOf('bookmark-hits-scroll') !== -1;
    // After this then this.getHitsScrollContainer() will work:

    // We are probably reviving a prerendered page
    if(theSearchHits && hitsScrollContainerAppearsSetup) {
      this.nodes.theSearchHits = theSearchHits;
      this.nodes.theHitsScrollContainer = hitsScrollContainer;
    } else if(theSearchHits && !hitsScrollContainer) {
      hitsScrollContainer = document.createElement('div');
      var hiddenClass = 'bookmark-hits-scroll bookmark-hits-scroll-hidden';
      hitsScrollContainer.className = hiddenClass;
      theSearchHits.appendChild(hitsScrollContainer);
      this.nodes.theSearchHits = theSearchHits;
      this.nodes.theHitsScrollContainer = hitsScrollContainer;
    } else if(theSearchHitsId) {
      console.error(
        'You supplied options searchHitsId but we could not find one of the elements ' + theSearchHitsId +
        '. Either that or something is wrong with the pre-rendering of the page'
      );
    }
    
    /**
     * Prevent blur from any existing controls that already have focus by
     * preventDefault on mouseDown event.
     * You can still style the mouse down state by using the css :active
     * pseudo-class.
     */
    hitsScrollContainer.addEventListener('mousedown', function(e) {e.preventDefault();});
  };

  Runner.prototype.getItemForCursor = function(i) {
    var hitsScrollContainer = this.getHitsScrollContainer();
    var maybeHitsList = hitsScrollContainer.childNodes[0];
    return maybeHitsList.className.indexOf('bookmark-hits-list') === -1 ? null :
      maybeHitsList.childNodes[i];
  }
  // alert('TODO: When clicking on document, set the active mode to null - let compeonts decide what they want to do when they are no longer the active mode. Dropdowns can reset their querystate to NONE. Autocompletes would not. Then make it so that all components get a notification for any active state transition away from them (or maybe even to them).');
  Runner.prototype.shouldSearchBeVisible = function(activeSearchComponent) {
    if(!activeSearchComponent) {
      return false;
    }
    if(this.searchState.userRequestedCloseEvenIfActive) {
      return false;
    } else {
      return true;
      // return activeSearchComponent.queryState !== QueryStates.NONE_AND_HIDE;
    }
  };
  Runner.prototype.setupSearchInput = function() {
    var doc = this;
    var theSearchFormId = doc.searchFormId;
    if(theSearchFormId) {
      var theSearchForm = document.getElementById(theSearchFormId);
      doc.searchState.CONTENT = new TextDocSearch({
        root: theSearchForm,
        pages: doc.pageState,
        /**
         * When input is blurred we do not set the active component to null.
         */
        onBlur: function inputBlur(e) {
          console.log('blur input');
        },
        // TODO: Rembember last focused element so that escape can jump back to it.
        // Ctrl-c can toggle open, and Esc can toggle open + focus.
        // When hitting enter it can reset the "last focused" memory.
        onFocus: function doInputFocus(e) {
          if(window['bookmark-header']) {
            window['bookmark-header'].scrollIntoView({behavior: 'smooth'});
          }
        },
        onDoesntWantActiveStatus: function(comp) {
          console.log('search input doesnt want');
          if(doc.searchState.activeSearchComponent === comp) {
            doc.setActiveSearchComponent(null);
            doc.updateSearchHitsVisibility(doc.searchState.activeSearchComponent);
          }
        },
        /**
         * When the component wants the popup menu to be shown for it, and it
         * has a useful (or new) .getQuery() that can be polled.
         */
        onWantsToHaveActiveStatus: function(comp) {
          doc.setActiveSearchComponent(comp);
          // Upon focus, reselect the first result cursor, otherwise keep old one
          console.log("text input wants to have active status");
          doc.runSearchWithInputValue();
        },
        onKeydown: function(e) {return doc.handleSearchComponentKeydown(doc.searchState.CONTENT, e)},
        /**
         * Allow components to test if they are the active component.
         */
        isActiveComponent: function() {
          return doc.searchState.activeSearchComponent === doc.searchState.CONTENT;
        }
      });
    }
  };

  Runner.prototype.searchDocsWithActiveSearchComponent = function(query, renderTopRow, contentRoot) {
    var doc = this;
    var searchComponent = doc.searchState.activeSearchComponent;
    hierarchicalIndexForSearch(doc.pageState)
    var hits = [];
    console.log(searchComponent.queryState);
    if(searchComponent.queryState === QueryStates.ALL) {
      console.log('noop filter', doc.pageState);
      return noopHierarchicalFilter(doc.pageState, renderTopRow).map(matchedSearchableToHit);
    } else if(searchComponent.queryState === QueryStates.FILTER) {
      var filteredHierarchicalSearchables = filterHierarchicalSearchables(query, doc.pageState, renderTopRow);
      hits = filteredHierarchicalSearchables.map(matchedSearchableToHit);
      return hits;
    } else {
      console.error('Unknown query state', searchComponent.queryState, 'for component', searchComponent);
    }
  };


  Runner.prototype.runSearchWithInputValue = function () {
    var doc = this;
    var theTextDocSearch = doc.searchState.CONTENT;
    if(doc.searchState.activeSearchComponent === theTextDocSearch) {
      var query = theTextDocSearch.getQuery();
      var results = doc.searchDocsWithActiveSearchComponent(
        query,
        doc.topRowForDocSearch.bind(doc)
      );
      doc.searchState.activeSearchComponent.results = results;
      doc.updateSearchResultsList(doc.searchState.activeSearchComponent, query, results, doc.standardResultsClickHandler.bind(doc));
      doc.updateSearchHitsVisibility(doc.searchState.activeSearchComponent);
    }
  };

  Runner.prototype.setupVersionButton = function () { 
    var doc = this;
    if(this.versionButtonId && this.versionPageIs) {
      var versionMenuButton = document.getElementById(this.versionButtonId);
      var versionContentsContainer =
        this.pageState[this.versionPageIs].contentContainerNode;
      if(!versionMenuButton) {
        console.error('Version menu selector/content with id ', this.versionButtonId, ' doesnt exist');
      }
      if(!versionContentsContainer) {
        console.error(
          'Page for config option "versionPageIs" does not exist: ',
          this.versionButtonId,
          '. There should be a page key in your "pages" config with that name'
        );
      }
      this.searchState.VERSIONS = new TextDocSelector({
        root: versionMenuButton,
        pages: Flatdoc.keepOnly(doc.pageState, (pageData, pageKey) => this.versionPageIs.toLowerCase() === pageKey),
        onKeydown: function(e) {return this.handleSearchComponentKeydown(doc.searchState.VERSIONS, e)}.bind(this),
        onWantsToHaveActiveStatus: function(comp) {
          doc.setActiveSearchComponent(comp);
          // Upon focus, reselect the first result cursor, otherwise keep old one
          console.log("version selector wants to have active status");
          doc.runVersionsSearch();
        },
        /**
         * Allow components to test if they are the active component.
         */
        isActiveComponent: function() {
          return doc.searchState.activeSearchComponent === doc.searchState.VERSIONS;
        },
        onDoesntWantActiveStatus: function(comp) {
          console.log('vversion doesnt want');
          if(doc.searchState.activeSearchComponent === comp) {
            doc.setActiveSearchComponent(null);
            doc.updateSearchHitsVisibility(doc.searchState.activeSearchComponent);
          }
        },
        onBlur: function inputBlur(e) {
          console.log('blur input');
        },
        // TODO: Rembember last focused element so that escape can jump back to it.
        // Ctrl-c can toggle open, and Esc can toggle open + focus.
        // When hitting enter it can reset the "last focused" memory.
        onFocus: function doInputFocus(e) {
          if(window['bookmark-header']) {
            window['bookmark-header'].scrollIntoView({behavior: 'smooth'});
          }
        },
      });
    }
  };
  Runner.prototype.updateSearchHitsVisibility = function(searchComponent) {
    console.log('updateSearchHitsVisibility');
    var hitsScrollContainer = this.nodes.theHitsScrollContainer;
    if(!this.shouldSearchBeVisible(searchComponent)) {
      hitsScrollContainer.className = 'bookmark-hits-scroll bookmark-hits-scroll-hidden';
      return false;
    } else {
      hitsScrollContainer.className = 'bookmark-hits-scroll';
      return true;
    }
  };
  Runner.prototype.handleSearchComponentKeydown = function(searchComponent, evt) {
    var doc = this;
    // alert('need to make sure the active component is set here');
    var effectiveCursorPosition = doc.effectiveCursorPosition(searchComponent);
    var isVisible = doc.shouldSearchBeVisible(searchComponent);
    var nextIndex;
    if (evt.keyCode === 40 /* down */) {
      if(!isVisible && searchComponent.getQuery() !== "") {
        // Promote to zero on first down if neg one
        nextIndex = Math.max(searchComponent.userRequestedCursor, 0);
        doc.searchState.userRequestedCloseEvenIfActive = false;
      } else {
        nextIndex = (effectiveCursorPosition < searchComponent.results.length - 1)
          ? effectiveCursorPosition + 1
          : searchComponent.userRequestedCursor;
      }
    }
    if (evt.keyCode === 38 /* up */) {
      if(effectiveCursorPosition !== -1) {
        nextIndex = effectiveCursorPosition - 1;
      } else {
        nextIndex = searchComponent.userRequestedCursor;
      }
    }
    if (evt.keyCode === 38 || evt.keyCode === 40) {
      searchComponent.userRequestedCursor = nextIndex;
    }
    if(isVisible && evt.keyCode === 13) {  // enter
      var itemForCursor = doc.getItemForCursor(effectiveCursorPosition);
      $(itemForCursor).trigger('click');
    } else if(!isVisible && evt.keyCode === 13) {
      doc.searchState.userRequestedCloseEvenIfActive = false;
    } else if(evt.keyCode === 27) {
      // console.log('local escape');
      // // Let's make escape close and blur
      // $(theSearchInput).blur();
      // doc.searchState.userRequestedCloseEvenIfActive = !doc.searchState.userRequestedCloseEvenIfActive;
      // doc.updateSearchHitsVisibility(searchComponent);
    } else if(evt.keyCode === 67 && evt.ctrlKey) {    // esc or ctrl-c
      // But ctrl-c can toggle without losing focus
      // doc.searchState.userRequestedCloseEvenIfActive = !doc.searchState.userRequestedCloseEvenIfActive;
      // doc.updateSearchHitsVisibility(searchComponent);
    }
    
    // Either way, visible or not - if enter is pressed, prevent default.
    // Because a "required" form field that is empty will submit on enter and
    // then make an ugly Chrome popup saying "this is required".
    if (evt.keyCode === 38 || evt.keyCode === 40 || evt.keyCode === 13) {
      evt.preventDefault();
      doc.updateSearchResultsList(
        searchComponent,
        searchComponent.getQuery(),
        searchComponent.results,
        doc.standardResultsClickHandler.bind(doc)
      );
    }
  };
  Runner.prototype.deepestContextWithSlug = function(context) {
    return context.h6 && context.h6.id ? context.h6 :
           context.h5 && context.h5.id ? context.h5 :
           context.h4 && context.h4.id ? context.h4 :
           context.h3 && context.h3.id ? context.h3 :
           context.h2 && context.h2.id ? context.h2 :
           context.h1 && context.h1.id ? context.h1 : null;

  };
  Runner.prototype.standardResultsClickHandler = function(searchComponent, query, results, i, e) {
    var doc = this;
    searchComponent.userRequestedCursor = i;
    // doc.searchState.userRequestedCloseEvenIfActive = true;
    // doc.updateSearchHitsVisibility();
    doc.updateSearchResultsList(searchComponent, query, results, doc.standardResultsClickHandler.bind(doc));
    var node = results[i].matchedSearchable.searchable.node;
    $.highlightNode(node);
    customScrollIntoView({
      smooth: true,
      container: 'page',
      element: node,
      mode: 'top',
      topMargin: 2 * headerHeight,
      bottomMargin: 0
    });
  };
  Runner.prototype.setupSearch = function() {
    var doc = this;
    doc.setupSearchInput();
    doc.setupVersionButton();
    doc.setupHitsScrollContainer();
    var theTextDocSearch = doc.searchState.CONTENT;
    var theSearchHits = doc.nodes.theSearchHits;
    if(!theTextDocSearch || !theSearchHits) {
      return;
    }
    var hitsScrollContainer = doc.nodes.theHitsScrollContainer;
    doc.nodes.theSearchHits.style.cssText += "position: sticky; top: " + (headerHeight - 1) + "px; z-index: 100;"
    function setupGlobalKeybindings() {
      window.document.body.addEventListener('keypress', e => {
        if(!theTextDocSearch.isFocused() && e.key === "/") {
          theTextDocSearch.focus()
          theTextDocSearch.selectAll()
          e.preventDefault();
         }
      });
    }
    document.addEventListener('keydown', function (evt) {
      if(evt.keyCode === 27) {
        console.log('global escape');
        // Let's make escape close and blur
        if(theTextDocSearch.isFocused()) {
          theTextDocSearch.blur();
        }
        if(!doc.searchState.userRequestedCloseEvenIfActive) {
          doc.searchState.userRequestedCloseEvenIfActive = true;
        }
        doc.setActiveSearchComponent(null);
        // Maybe updateSearchHitsVisibility should happen in setActiveSearchComponent.
        doc.updateSearchHitsVisibility(doc.searchState.activeSearchComponent);
      } else if(evt.keyCode === 67 && evt.ctrlKey) {    // esc or ctrl-c
        // But ctrl-c can toggle without losing focus
        doc.searchState.userRequestedCloseEvenIfActive = !doc.searchState.userRequestedCloseEvenIfActive;
        doc.updateSearchHitsVisibility(doc.searchState.activeSearchComponent);
      }
      // alert('todo have ctrl-c keep the current active component, but tell that component to go to QueryMode.NONE_AND_HIDE');
    });
    setupGlobalKeybindings();

    function onGlobalClickOff(e) {
      doc.setActiveSearchComponent(null);
      // We'll consider all other search modes to be "ephemeral".
      doc.updateSearchHitsVisibility(null);
      // e.stopPropagation();
    }
    document.querySelectorAll('.bookmark-content-root')[0].addEventListener('click', onGlobalClickOff);
  };

  Runner.prototype.topRowForVersionSearch = function(context, node) {
    var topRow = this.topRowForDocSearch(context, node);
    var tagsDiv = topRow.childNodes[1];
    var tag = document.createElement('div');
    tag.className = 'bookmark-hits-item-contents-top-row-tags-tag';
    tag.innerText = "Latest";
    tagsDiv.appendChild(tag);
    return topRow;
  };
  Runner.prototype.runVersionsSearch = function runVersionsSearch() {
    var doc = this;
    var searchComponent = doc.searchState.VERSIONS;
    if(window['bookmark-header']) {
      window['bookmark-header'].scrollIntoView({behavior: 'smooth'});
    }
    doc.setActiveSearchComponent(searchComponent);
    console.log('running version search');
    // TODO: Reset this to NONE on blur/selection etc.
    doc.updateSearchHitsVisibility(doc.searchState.VERSIONS);
    var results = doc.searchDocsWithActiveSearchComponent(
      searchComponent.getQuery(),
      doc.topRowForVersionSearch.bind(doc)
    );
    searchComponent.results = results;
    doc.updateSearchResultsList(searchComponent, searchComponent.getQuery(), results, doc.standardResultsClickHandler.bind(doc));
  };

  Runner.prototype.makeCodeTabsInteractive = function() {
    $('codetabbutton').each(function(i, e) {
      var forTabContainerId = e.dataset.forContainerId;
      var index = e.dataset.index;
      $(e).on('click', function(evt) {
        var tabContainer = e.parentNode;
        console.log('searching this query what: $("' + '#' + forTabContainerId + ' codetabbutton")');
        $(e).addClass('bookmark-codetabs-active');
        $(tabContainer).removeClass('bookmark-codetabs-active1');
        $(tabContainer).removeClass('bookmark-codetabs-active2');
        $(tabContainer).removeClass('bookmark-codetabs-active3');
        $(tabContainer).removeClass('bookmark-codetabs-active4');
        $(tabContainer).removeClass('bookmark-codetabs-active5');
        $(tabContainer).addClass('bookmark-codetabs-active' + index);
      });
    });
  };

  
  /**
   * Remove any nodes that are not needed once rendered. This way when
   * generating a pre-rendered `.rendered.html`, they won't become part of the
   * bundle, when that rendered page is turned into a `.html` bundle. They have
   * served their purpose. Add `class='removeFromRenderedPage'` to anything you
   * want removed once used to render the page. (Don't use for script tags that
   * are needed for interactivity).
   */
  Runner.prototype.removeFromRenderedPage = function() {
    $('.removeFromRenderedPage').each(function(i, e) {
      e.parentNode.removeChild(e);
    });
  };

  /**
   * See documentation for `continueRight` css class in style.styl.
   */
  Runner.prototype.fixupAlignment = function() {
    document.querySelectorAll(
       // TODO: Add the tabs container here too.
       '.bookmark-content > img + pre,' +
       '.bookmark-content > img + blockquote,' +
       '.bookmark-content > p + pre,' +
       '.bookmark-content > p + blockquote,' +
       '.bookmark-content > ul + pre,' +
       '.bookmark-content > ul + blockquote,' +
       '.bookmark-content > ol + pre,' +
       '.bookmark-content > ol + blockquote,' +
       '.bookmark-content > h0 + pre,' +
       '.bookmark-content > h0 + blockquote,' +
       '.bookmark-content > h1 + pre,' +
       '.bookmark-content > h1 + blockquote,' +
       '.bookmark-content > h2 + pre,' +
       '.bookmark-content > h2 + blockquote,' +
       '.bookmark-content > h3 + pre,' +
       '.bookmark-content > h3 + blockquote,' +
       '.bookmark-content > h4 + pre,' +
       '.bookmark-content > h4 + blockquote,' +
       '.bookmark-content > h5 + pre,' +
       '.bookmark-content > h5 + blockquote,' +
       '.bookmark-content > h6 + pre,' +
       '.bookmark-content > h6 + blockquote,' +
       '.bookmark-content > table + pre,' +
       '.bookmark-content > table + blockquote'
     ).forEach(function(e) {
       // Annotate classes for the left and right items that are "resynced".
       // This allows styling them differently. Maybe more top margins.  TODO:
       // I don't think that bookmark-synced-up-left is needed. continueRight
       // seems to do the trick and the css for bookmark-synced-up-left seems to
       // just ruin it actually.
       e.className += 'bookmark-synced-up-right';
       if(e.previousSibling) {
          e.previousSibling.className += 'bookmark-synced-up-left';
       }
     })

  };

  Runner.prototype.setupLeftNavScrollHighlighting = function() {
    var majorHeaders = $("h2, h3");
    majorHeaders.length && majorHeaders.scrollagent(function(cid, pid, currentElement, previousElement) {
      console.log('setting up scroll watchers for pid', pid);
      if (pid) {
       $("[href='#"+hashForLinkifiedId(pid)+"']").removeClass('active');
      }
      if (cid) {
       $("[href='#"+hashForLinkifiedId(cid)+"']").addClass('active');
      }
    });
  };
  Runner.prototype.handleHashChange = function(hash) {
    if (hash !== ''){
      console.log('hash changed', hash);
      if(hash[0] === '#') {
        hash = hash.substring(1);
        anchorJump('#' + linkifidIdForHash(hash));
      }
    }
  };
  Runner.prototype.waitForImages = function() {
    var onAllImagesLoaded = function() {
      // Has to be done after images are loaded for correct detection of position.
      this.setupLeftNavScrollHighlighting();
      window.addEventListener('hashchange', function(e) {
        this.handleHashChange(location.hash);
      }.bind(this));
      // Rejump after images have loaded
      this.handleHashChange(location.hash);

      /**
       * If you add a style="visibility:hidden" to your document body, we will clear
       * the style after the styles have been injected. This avoids a flash of
       * unstyled content.
       * Only after scrolling and loading a stable page with all styles, do we
       * reenable visibility.
       * TODO: This is only needed if there is a hash in the URL. Otherwise,
       * we can show the page immediately, non-blocking since we don't need to scroll
       * to the current anchor. (We don't need to wait for images to load which are
       * likely below the fold). This assumes we can implement a header that is scalable
       * entirely in css. As soon as styles are loaded, the visibility can be shown.
       */
      console.log('all images loaded at', Date.now());
      if(window.location.hash) {
        document.body.style="visibility: visible";
     } else {
        console.log('Lack of hash in URL saved time in display:', Date.now() - window._bookmarkTimingStyleReady);
     }
    }.bind(this);

    var imageCount = $('img').length;
    var nImagesLoaded = 0;
    // Wait for all images to be loaded by cloning and checking:
    // https://cobwwweb.com/wait-until-all-images-loaded
    // Thankfully browsers cache images.
    function onOneImageLoaded(loadedEl) {
      nImagesLoaded++;
      if (nImagesLoaded == imageCount) {
        onAllImagesLoaded();
      }
    }
    if(imageCount === 0) {
        onAllImagesLoaded();
    } else {
      $('img').each(function(_i, imgEl) {
        $('<img>').on('load', onOneImageLoaded).attr('src', $(imgEl).attr('src'));
        $('<img>').on('error', onOneImageLoaded).attr('src', $(imgEl).attr('src'));
      });
    }
  };

  Runner.prototype.run = function(
      onCurrentRenderPageDone, onAllRenderPagesDone, onNextIndexPageDone, onAllIndexPagesDone) {
  };

  /**
   * Loads the Markdown document (via the fetcher), parses it, and applies it
   * to the elements.
   */
  Runner.prototype.run = function(
      onCurrentRenderPageDone, onAllRenderPagesDone, onNextIndexPageDone, onAllIndexPagesDone) {
    var start = Date.now();
    var doc = this;
    $(doc.pageRootSelector).trigger('flatdoc:loading');
    $(doc.pageRootSelector).on('flatdoc:ready', this.makeCodeTabsInteractive);
    $(doc.pageRootSelector).on('flatdoc:ready', this.removeFromRenderedPage.bind(this));
    $(doc.pageRootSelector).on('flatdoc:ready', this.fixupAlignment.bind(this));
    $(doc.pageRootSelector).on('flatdoc:ready', this.waitForImages.bind(this));
    // If this *is* an already rendered snapshot, then no need to render
    // anything. Just fire off the ready events so that hacky jquery code can
    // perform resizing etc.
    $(doc.pageRootSelector).on('flatdoc:ready', function(e) {
      doc.makeCodeTabsInteractive();
      doc.setupSearch();
      // Need to focus the window so global keyboard shortcuts are heard.
      $(window).focus()
    });
    if(document.body.className.indexOf('bookmark-already-rendered') !== -1) {
      $(doc.pageRootSelector).trigger('flatdoc:style-ready');
      $(doc.pageRootSelector).trigger('flatdoc:ready');
      return;
    }
    document.body.className += ' bookmark-already-rendered';
    var stylusFetchedYet = !doc.stylusFetcher;
    var allDocsFetchedYet = false;
    var everythingFetchedYet = false;
    var stylusResult = null;
    function handleDones() {
      var foundUnfetchedDoc = false;
      Flatdoc.forEachPage(
        doc.pageState, 
        function(chapData, _) {
          foundUnfetchedDoc = foundUnfetchedDoc || chapData.markdownAndHeader === null;
        }
      );
      var wasEverythingFetchedYetBefore = allDocsFetchedYet && stylusFetchedYet;
      if(!allDocsFetchedYet && !foundUnfetchedDoc) {
        allDocsFetchedYet = true;
        doc.handleDocsFetched();
      }
      if(!stylusFetchedYet && !!stylusResult) {
        stylusFetchedYet = true;
        $(doc.pageRootSelector).trigger('flatdoc:style-ready');
      }
      everythingFetchedYet = allDocsFetchedYet && stylusFetchedYet;
      if(everythingFetchedYet && !wasEverythingFetchedYetBefore) {
        $(doc.pageRootSelector).trigger('flatdoc:ready');
      }
    };
    
    var fetchOne = function(fetcher, cb) {
      fetcher(function(err, md) {
        if(err) {
          cb(err, null);
          return;
        }
        var markdown = normalizeMarkdownResponse(md);
        var markdownNormalizedCodeTabs = normalizeDocusaurusCodeTabs(markdown);
        var markdownNormalizedYaml = normalizeYamlMarkdownComments(markdownNormalizedCodeTabs);
        var markdownAndHeader = parseYamlHeader(markdownNormalizedYaml, window.location.pathname);
        // Parse out the YAML header if present.
        var data = markdownAndHeader;

        /* Flatdoc.parser.parse(
          doc,
          markdownAndHeader,
          doc.highlight, 
          pageState,
          pageKey
        ); */
        // About 258
        cb(err, data);
      });
    };
    Flatdoc.forEachPage(
      doc.pageState, 
      function(pageData, pageKey) {
        fetchOne(pageData.fetcher, function(err, data) {
          doc.pageState[pageKey].markdownAndHeader = data; 
          err && console.error(
            '[Flatdoc] fetching Markdown data failed for page:' + pageKey + '.',
            err
          );
          handleDones();
        })
      },
    );
    if(doc.stylusFetcher) {
      var templateFetchStart = Date.now();
      doc.stylusFetcher(function(err, stylusTxt) { // Will run sync
        doc.renderAndInjectStylus(err, stylusTxt, function(res) {
          stylusResult = res;
          handleDones();
        });
      });
    }
  };

  Runner.prototype.renderAndInjectStylus = function(err, stylusTxt, cb) {
    if(err) {
      console.error('[Flatdoc] fetching Stylus data failed.', err);
      cb('');
    } else {
      window.stylus.render(stylusTxt, function(err, result) {
        if(err) {
          console.error('Stylus error:' + err.message);
          cb('');
        } else {
          var style = document.createElement('style');
          style.type = 'text/css';
          style.name = 'style generated from .styl.html file';
          style.innerHTML = result;
          document.getElementsByTagName('head')[0].appendChild(style);
          cb(stylusTxt);
        }
      });
    }
  };

  Runner.prototype.handleDocsFetched = function() {
    var runner = this;
    function appendExperience(pageKey, pageData) {
      var markdownAndHeader = pageData.markdownAndHeader;

      marked = exports.marked;

      Parser.setMarkedOptions(runner.highlight);

      var premangledContent = $("<div>" + marked(markdownAndHeader.markdown));
      var title = markdownAndHeader.headerProps.title;
      if(!title) {
        title = premangledContent.find('h1').eq(0).text();
      }
      var pageClassName = 'page-' + pageKey;
      var containerForPageContent = document.createElement('div');
      containerForPageContent.className = 'bookmark-content ' + pageClassName;
      if(markdownAndHeader.headerProps.title) {
        var titleForPage = document.createElement('h0');
        titleForPage.className = 'bookmark-content-title ' + pageClassName;
        // Prepend the title to the main content section so it matches the style
        // of content (indentation etc).
        titleForPage.innerText = markdownAndHeader.headerProps.title;
        containerForPageContent.appendChild(titleForPage);
        if(markdownAndHeader.headerProps.subtitle) {
          var subtitleForPage = document.createElement('p');
          subtitleForPage.className = 'bookmark-content-subtitle';
          containerForPageContent.appendChild(subtitleForPage);
          subtitleForPage.innerText = markdownAndHeader.headerProps.subtitle;
        }
      }
      containerForPageContent.appendChild(premangledContent[0]);
      
      var menuBarForPage = document.createElement('div');
      menuBarForPage.className = 'bookmark-menubar ' + pageClassName;
      var premangledMenuForPage = document.createElement('div');
      premangledMenuForPage.className = 'bookmark-menu section ' + pageClassName;
      menuBarForPage.appendChild(premangledMenuForPage);
      var nonBlankContent = $(premangledContent).find('>*');

      var menu = Transformer.getMenu(runner, premangledContent)
      Array.prototype.forEach.call(nonBlankContent, (itm) => containerForPageContent.appendChild(itm));
      Array.prototype.forEach.call(MenuView(menu), (itm) => premangledMenuForPage.appendChild(itm));

      
      Transformer.buttonize(containerForPageContent);
      Transformer.smartquotes(containerForPageContent);
      
      var hierarchicalDoc = hierarchize(containerForPageContent);
      // Mangle content
      
      Transformer.addIDsToHierarchicalDoc(runner, hierarchicalDoc, pageKey);
      Transformer.mangle(runner, pageKey, hierarchicalDoc);
      
      // It's mutated.
      var mangledContentForPage = containerForPageContent;
      return {
        ...pageData,
        contentContainerNode: mangledContentForPage,
        menuContainerNode: menuBarForPage,
        hierarchicalDoc: hierarchicalDoc
      };
    }
    runner.pageState = Flatdoc.mapPages(
      runner.pageState, 
      function(data, pageKey) {
        return appendExperience(pageKey, data);
      }
    );
    runner.appendDocNodesToDom(runner.pageState);
    runner.activatePageForCurrentUrl(runner.pageState);
  };

  Runner.prototype.appendDocNodesToDom = function(data) {
    var contentRootNode = $('.bookmark-content-root')[0];
    var append = function(data, _) {
      contentRootNode.appendChild(data.contentContainerNode)
      contentRootNode.appendChild(data.menuContainerNode)
    };
    Flatdoc.forEachPage(data, append, append);
  };

  Runner.prototype.activatePageForCurrentUrl = function(data) {
    var dataForUrl = pageDataForUrl(window.location, data);
    var toggleClasses = function(data, _) {
      dataForUrl === data ?
        data.contentContainerNode.classList.add("current") :
        data.contentContainerNode.classList.remove("current");
      dataForUrl === data ?
        data.menuContainerNode.classList.add("current") :
        data.menuContainerNode.classList.remove("current");
    };
    Flatdoc.forEachPage(data, toggleClasses, toggleClasses);
  };

  /**
   * Fetches a given element from the DOM.
   *
   * Returns a jQuery object.
   * @api private
   */

  Runner.prototype.el = function(aspect) {
    return $(this[aspect], document.body);
  };

  /*
   * Helpers
   */

  // http://stackoverflow.com/questions/298750/how-do-i-select-text-nodes-with-jquery
  function getTextNodesIn(el) {
    var exclude = 'iframe,pre,code';
    return $(el).find(':not('+exclude+')').andSelf().contents().filter(function() {
      return this.nodeType == 3 && $(this).closest(exclude).length === 0;
    });
  }

  // http://www.leancrew.com/all-this/2010/11/smart-quotes-in-javascript/
  function quotify(a) {
    a = a.replace(/(^|[\-\u2014\s(\["])'/g, "$1\u2018");        // opening singles
    a = a.replace(/'/g, "\u2019");                              // closing singles & apostrophes
    a = a.replace(/(^|[\-\u2014\/\[(\u2018\s])"/g, "$1\u201c"); // opening doubles
    a = a.replace(/"/g, "\u201d");                              // closing doubles
    a = a.replace(/\.\.\./g, "\u2026");                         // ellipses
    a = a.replace(/--/g, "\u2014");                             // em-dashes
    return a;
  }
})(jQuery);

/* jshint ignore:start */

/*!
 * base64.js
 * http://github.com/dankogai/js-base64
 * THERE's A PROBLEM LOADING THIS in entrypoint mode.
 */


/**
 * marked - a markdown parser
 * Copyright (c) 2011-2020, Christopher Jeffrey. (MIT Licensed)
 * https://github.com/markedjs/marked
 */

/*!
 * node-parameterize 0.0.7
 * https://github.com/fyalavuz/node-parameterize
 * Exported as `Flatdoc.slugify`
 */


/* jshint ignore:end */

// This } is for the initial if() statement that bails out early.
}
