class ProgressBar extends HTMLElement
  createdCallback: ->
    @appendChild @span = document.createElement 'span'

  setProgress: (progress) ->
    @span.style.setProperty 'width', "#{progress * 100}%"
    if progress <= 0
      @classList.remove 'visible'
    else
      @classList.add 'visible'


ProgressBarElement =
  document.registerElement 'ide-reason-progress-bar',
    prototype: ProgressBar.prototype

module.exports = ProgressBarElement
