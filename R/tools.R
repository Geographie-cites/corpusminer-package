

favicon <- function(){
  tags$link(rel="shortcut icon", href="favicon.png")
}

ga_tracker <- function(id = "UA-40299595-6"){
  code <- sprintf( "
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

    ga('create', '%s', 'auto');
    ga('send', 'pageview');
  ", id )
  tags$script(HTML(code))
}

google_font <- function(family = "Orbitron|Cabin:400,700"){
  txt <- sprintf( "@import url('//fonts.googleapis.com/css?family=%s');", family )
  tags$style(HTML(txt))
}

cybergeo_head <- function(){
  tags$head(
    favicon(),
    ga_tracker( id = "UA-40299595-6"),
    google_font( family = "Orbitron|Cabin:400,700")
  )
}
