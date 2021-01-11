/*!
 * Flatdoc - (c) 2013, 2014 Rico Sta. Cruz
 * http://ricostacruz.com/flatdoc
 * @license MIT
 */

(function($) {

  var $window = $(window);
  var $document = $(document);

  /*
   * If the hash is empty, we don't need to scroll to anything, and therefore
   * we don't need to wait until images load to reveal the body (for size
   * issues).
   */
  $document.on('flatdoc:style-ready', function() {
    window._bookmarkTimingStyleReady = Date.now();
    if(!window.location.hash) {
      document.body.style="visibility: visible";
    }
  });

  /*
   * Scrollspy.
   */

  $document.on('flatdoc:ready', function() {
    if (typeof mediumZoom !== 'undefined') {
      mediumZoom(document.querySelectorAll('.bookmark-content img'), {
        scrollOffset: 20,
        container: document.body,
        margin: 24,
        background: '#ffffff',
      });
      document.querySelectorAll('.bookmark-content img').forEach(function(img) {
        var parent = img.parentElement;
        if (parent && parent.tagName.toUpperCase() === 'P') {
          // Allows targeting css for containers of images
          // since has() selector is not yet supported in css
          parent.className += ' imageContainer';
        } 
      });
    }
  });



})(jQuery);
/*! jQuery.scrollagent (c) 2012, Rico Sta. Cruz. MIT License.
 *  https://github.com/rstacruz/jquery-stuff/tree/master/scrollagent */

// Call $(...).scrollagent() with a callback function.
//
// The callback will be called everytime the focus changes.
//
// Example:
//
//      $("h2").scrollagent(function(cid, pid, currentElement, previousElement) {
//        if (pid) {
//          $("[href='#"+pid+"']").removeClass('active');
//        }
//        if (cid) {
//          $("[href='#"+cid+"']").addClass('active');
//        }
//      });

(function($) {

  $.fn.scrollagent = function(options, callback) {
    // Account for $.scrollspy(function)
    if (typeof callback === 'undefined') {
      callback = options;
      options = {};
    }

    var $sections = $(this);
    var $parent = options.parent || $(window);

    // Find the top offsets of each section
    var offsets = [];
    $sections.each(function(i) {
      var offset = $(this).attr('data-anchor-offset') ?
        parseInt($(this).attr('data-anchor-offset'), 10) :
        (options.offset || 0);

      offsets.push({
        id: $(this).attr('id'),
        index: i,
        el: this,
        offset: offset
      });
    });

    // State
    var current = null;
    var height = null;
    var range = null;

    // Save the height. Do this only whenever the window is resized so we don't
    // recalculate often.
    $(window).on('resize', function() {
      height = $parent.height();
      range = $(document).height();
    });

    // Find the current active section every scroll tick.
    $parent.on('scroll', function() {
      var y = $parent.scrollTop();
      // y += height * (0.3 + 0.7 * Math.pow(y/range, 2));

      var latest = null;

      for (var i in offsets) {
        if (offsets.hasOwnProperty(i)) {
          var offset = offsets[i];
          var el = offset.el;
          var relToViewport = offset.el.getBoundingClientRect().top;
          if(relToViewport > 0 && relToViewport < height / 2) {
            latest = offset;
            break;
          }
        }
      }

      if (latest && (!current || (latest.index !== current.index))) {
        callback.call($sections,
          latest ? latest.id : null,
          current ? current.id : null,
          latest ? latest.el : null,
          current ? current.el : null);
        current = latest;
      }
    });

    $(window).trigger('resize');
    $parent.trigger('scroll');

    return this;
  };

})(jQuery);
