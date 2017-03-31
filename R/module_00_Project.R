
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
    google_font( family = "Orbitron|Cabin:400,700"),
    tags$link(rel="stylesheet", href="cybergeo.css")
  )
}

#' @export
cybergeo_module_project_UI <- function(id){

  tabPanel( "The Project",
    cybergeo_head(),
    fluidRow(
      column(9,
        h1("Cybergeo | 1996-2016", style = "font-family: 'Orbitron', sans-serif; font-weight: 500; line-height: 1.1; color: #ffffff;"),
        tags$p(class="text-justify", "Cybergeo turns 20: it’s time to look back for reflection and to anticipate future evolution!", a("cybergeo.revues.org/",href="http://cybergeo.revues.org/"), br()),
        h3("The editorial policy"),
        tags$p(class="text-justify",
              "First entirely electronic journal for social sciences in the world, peer reviewed, European, open (free of charge for authors and readers), with a focus on geography and widely open to the diversity of research agendas and methodologies in all countries.
              Cybergeo is a success story with now more than one million papers downloaded every year."),
        br(),
        h3("An app to look back"),
        tags$p(class="text-justify", "This app builds on 20 years of publication in Cybergeo.
              You can play with data, drawing geographical networks of authoring,
              studying and citing through countries, analyzing semantic networks per key words and articles’ content,
              you can review twenty years of epistemological and thematic trends in a variety of fields of scientific interest.
              The networks tell who studies what, where and how. Data are regularly updated."
        ),

        h3("About the app"),
        "All data, materials and source codes are freely available on this repository: ",
        a("github.com/Geographie-cites/cybergeo20",href="https://github.com/Geographie-cites/cybergeo20"),
        br(),

        h3("The Team"),
        "Pierre-Olivier Chasset", a("(@chasset)",href="https://github.com/chasset"), br(),
        "Hadrien Commenges", a("(@hcommenges)",href="https://github.com/hcommenges"), br(),
        "Clémentine Cottineau", a("(@ClementineCttn)",href="https://github.com/ClementineCttn"), br(),
        "Antoine Fleury", br(),
        "Christine Kosmopoulos", br(),
        "Denise Pumain", br(),
        "Juste Raimbault", a("(@JusteRaimbault)",href="https://github.com/JusteRaimbault")
      ) ,
      column(3,
        img(src = "favicon.png",class="img-responsive"))
      )
    )

}
