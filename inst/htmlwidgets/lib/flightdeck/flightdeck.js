$(document).ready(function(){
  $('a[data-page]').click(function(){
    console.log('clicked')
    $('.page').hide()
    console.log('#' + $(this).data('page'))
    $('#' + $(this).data('page')).show()
    window.dispatchEvent(new Event('resize'));
  })
})