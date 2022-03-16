if (window.matchMedia("(max-width: 768px)").matches) {
      var x = document.getElementById('panel_opciones');
      if (x.style.display === 'none') {
      x.style.display = 'block';
      } else {
      x.style.display = 'none';
      }
    } else {
      /* the view port is at least 768 pixels wide */
      /* show your dropdown and hide your link */
    }

  

$(document).ready(function() {
  
    $('.sidebar-toggle').click(function() {
        $('#panel_opciones').toggle();
    });
    
    
    
    
});





