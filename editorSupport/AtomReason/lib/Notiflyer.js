'use babel';
/*
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 */

/**
 * TODO: Create standalone package for Notiflyer.
 */
let successClasses = 'formatting-bar formatting-bar-success';
let infoClasses = 'formatting-bar formatting-bar-info';
let errorClasses = 'formatting-bar formatting-bar-error invert';
let feedbackBar = document.createElement('div');
let curFeedbackBarClass = null;
let isHovering = false;
let timeout = null;
let clearFeedbackBar = () => {
  if (curFeedbackBarClass && !isHovering) {
    feedbackBar.className = curFeedbackBarClass + ' notiflyer-animate-out';
    // feedbackBar.innerHTML = ""
    curFeedbackBarClass = null;
  }
};
let handleMouseEnter = (e) => {
  timeout && clearTimeout(timeout);
  isHovering = true;
};
let handleMouseLeave = (e) => {
  isHovering = false;
  timeout && clearTimeout(timeout);
  timeout = setTimeout(clearFeedbackBar, 2000);
};
let showSuccesBar = (msg) => {
  timeout && clearTimeout(timeout);
  feedbackBar.className = successClasses + ' notiflyer-animate';
  curFeedbackBarClass = successClasses;
  feedbackBar.innerHTML = "<span class='icon icon-code'></span><span> " + msg + "</span>";
  timeout = setTimeout(clearFeedbackBar, 2000);
};
let showInfoBar = (msg) => {
  timeout && clearTimeout(timeout);
  feedbackBar.className = infoClasses + ' notiflyer-animate';
  curFeedbackBarClass = infoClasses;
  feedbackBar.innerHTML = "<span class='icon icon-code'></span><span> " + msg + "</span>";
  timeout = setTimeout(clearFeedbackBar, 2000);
};
let showFailBar = (msg) => {
  timeout && clearTimeout(timeout);
  feedbackBar.className = errorClasses + ' notiflyer-animate';
  curFeedbackBarClass = errorClasses;
  feedbackBar.innerHTML = "<span class='icon icon-code'></span><span> " + msg + "</span>";
  timeout = setTimeout(clearFeedbackBar, 2000);
};
feedbackBar.className = 'formatting-bar';
feedbackBar.onmouseenter = handleMouseEnter;
feedbackBar.onmouseleave = handleMouseLeave;
module.exports = {
  feedbackBar,
  clearFeedbackBar,
  handleMouseEnter,
  handleMouseLeave,
  showSuccesBar,
  showFailBar,
  showInfoBar
};
