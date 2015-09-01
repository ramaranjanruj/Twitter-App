library(shiny)


shinyUI(fluidPage(
        
        
        
        headerPanel("Tweet Pundit - Get more from Tweets!"),
        
        # Getting User Inputs
        
        sidebarPanel(
                
                wellPanel(
                        textInput("entity1", "Handle 1: ","#sad"),
                        textInput ("entity2","Handle 2: ","#happy"),
                        HTML
                        ("<div style='font-size: 10px;font-weight: bold'> Enter the handle name
                        starting with '@' or hashtag starting with '#'</div>")
                        )  ,
                wellPanel(
                        dateInput("date", label = h4("Select date"), value = "2015-08-20"),
                        HTML
                        ("<div style='font-size: 12px;font-weight: bold'> Feel free to change the date! </div>")),
                wellPanel(
                        sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=10,max=300,value=150,step=1), # The max can, of course, be increased
                        actionButton(inputId='actb',icon =icon("twitter"), label="Search", color = "blue")
                        
                )
                
                ),
        
        mainPanel(
                #progressInit(),
                tabsetPanel(
                        
                        #Output from tab 4 ----So a box plot to see the distribution of scores of sentiments 
                        tabPanel("Sentiment Analysis", plotOutput("sentiboxplot"), HTML
                                 ("<div> This violin plot shows the distribution of the negetive and positive words in each of the tweets. The more shaded area in the positive zone signifies the tweet has more positive connotations and vice versa.</div>"),HTML("<div> PS: The tweets were cleaned before the analysis. The scores are generated as per Breen's Algorithm. For details please visit: .</div>"),
                                 HTML("<div><br><br><br><br><br></div>"),
                                 HTML("<div style='font-size: 12px;font-weight: bold'>Contact: ramaranjanruj@gmail.com</div>"),
                                 
                                 id="test"),
                        
                        #Output from tab 5 - Word clouds - with some html tags
                        
                        tabPanel("Word Clouds",h3(textOutput("entity1wc")),HTML("<div>Word Cloud of the first entity</div>"),plotOutput("entity1wcplot"),h3(textOutput("entity2wc")),HTML("<div>Word Cloud of the second entity</div>"),plotOutput("entity2wcplot")),
                        
                        #Output from tabs 6 and 7, the raw tweets
                        tabPanel("Entity 1 Raw tweets",tableOutput("tableentity1")),
                        tabPanel("Entity 2 Raw tweets",tableOutput("tableentity2"))
                )
        )
        
))