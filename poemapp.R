#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(stringr)
nouns <- read.table("~/nounlist", quote="\"", comment.char="")$V1
verbs <- read.table("~/verblist", quote="\"", comment.char="")$V1
conjun <- c("for","and","but","or","yet","so")
adj <- read.table("~/adjectives", quote="\"", comment.char="")$V1
adv <- read.table("~/adverbs", quote="\"", comment.char="")$V1
prep <- read.table("~/prepositions", quote="\"", comment.char="")$V1
pro1 <- c("I", "He", "She", "It")


# Title

makePoemTitle<- function(){
    n<-str_to_title(sample(nouns, 1))
    v<-str_to_title(sample(verbs,1))
    v <- paste(v,"ing", sep="")
    set <- c(n,v)
    fin <- sample(set,1)
    return(paste("The",fin))
}


## First sentence

makePoemStart <- function(){
    n <- sample(nouns,1)
    v <- sample(verbs,1)
    av <- sample(adv,1)
    ad <- sample(adj,1)
    return(paste("It","was",ad, "and",  paste(v, "ed",sep=""), av,sep=" " ))
}

nextline <- function(){
    c <- sample(conjun, 1)
    n <- sample(nouns,2)
    v<- sample(verbs,1)
    av <- sample(adv,1)
    return(paste(c,"the",n[1],"and","the",n[2], "it", paste(v,"s",sep=""), av))
}

thirdline <- function(){
    p <- sample(prep,1)
    n <- sample(nouns, 2)
    v <- sample(verbs,1)
    ad <- sample(adv,1)
    return(paste(p, "the", n[1], paste(v,"s",sep=""), "the", ad, n[2]))
}

finaline <- function(){
    n <- sample(nouns,1)
    av <- sample(adv,1)
    aj <- sample(adj,1)
    return(paste(av, "the", n, "is", aj))
}

ezpoem <- function(){
    t <- makePoemTitle()
    len <- sample(4:8,1)
    l <-rep(0,len)
    l[1] <- makePoemStart()
    for(i in 1:(len-2)){
        t1 <- nextline()
        t2 <- thirdline()
        l[i+1]<-sample(c(t1,t2),1)
    }
    l[len] <- finaline()
    plist <- list("Title"= t, "Poem" = paste(l, sep="\n"), "Length"=len)
    return(plist)
}

library(shiny)

# Define UI 
ui <- fluidPage(
    # Application title
    titlePanel("Skip's Poem Generator"),
    sidebarLayout(
        sidebarPanel(
            actionButton("button", "Generate")
        ),
        mainPanel(
            textOutput("title"),
            textOutput("poem")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$button,{
    output$title <- renderText({
        ezpoem()$Title})
    output$poem <- renderText({
        ezpoem()$Poem})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
