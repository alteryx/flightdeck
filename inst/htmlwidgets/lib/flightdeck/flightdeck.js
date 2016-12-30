$(document).ready(function(){
  $('a[data-page]').click(function(){
    console.log('clicked')
    $('.page').hide()
    console.log('#' + $(this).data('page'))
    $('#' + $(this).data('page')).show()
    window.dispatchEvent(new Event('resize'));
  })
  
   // confusion matrices
   $('.table.fd-confusion-matrix td').css('background', function(i, v){
     var v2 = $($(this).children('div')[0]).data('value')
     var v3 = $($(this).children('div')[0]).data('phrase')
     return v3 === 'correctly'  ? 
       'rgba(44, 160, 44, ' + v2 + ')' : 
       'rgba(214, 39, 40,  ' + v2 + ')'
  })
})