  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-73274243-1', 'auto');
  ga('send', 'pageview');
  
  // ga('send', 'event', 'category', 'action', 'label', value);
  
  $(document).on('change', 'select', function(e) {
    ga('send', 'event', 'Choose State', $(e.currentTarget).val(), $(e.currentTarget).val());
  });
    $(document).on('change', 'input', function(e) {
    ga('send', 'event', 'Change Slider', $(e.currentTarget).attr('id'), $(e.currentTarget).val());
  });