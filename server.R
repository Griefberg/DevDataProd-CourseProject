library(dplyr); library(shiny); library(ggplot2); library(caret); library(scales); library(rpart); library(e1071)

census <- read.table('data.txt', sep = ',', col.names = c('age', 'workclass', 'fnlwgt',
                                                        'education', 'education_num', 'marital_status',
                                                        'occupation', 'relationship', 'race',
                                                        'sex', 'capital_gain', 'capital_loss',
                                                        'hours_per_week', 'native_country', 'income'))
census <- select(census, -education_num, -capital_gain, -capital_loss, -fnlwgt, -workclass, -relationship)
for (i in 1:length(census)){
        if (class(census[,i]) == 'factor'){
                census[,i] <- as.factor(gsub(" ", "", census[,i]))
        }
        
}

shinyServer(function(input, output) {
        ###########EXPLORATORY  DATA ANALYSIS ###################
        # ALL INCOME GROUPs
        output$age <- renderPlot({
                hist(census$age, breaks=25, col="lightblue", xlab="Age", ylab="Frequency", main = 'Age Distribution')
        })
        output$sex <- renderPlot({
                freq_sex <- as.data.frame(round(table(census$sex)/ nrow(census) * 100, 2))
                ggplot(freq_sex, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Sex") +
                        theme(axis.text.x=element_blank()) + theme_bw() +
                        geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                                      label = percent(Freq/100)), size=5) + 
                        ggtitle("Gender structure") + theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$race <- renderPlot({
                freq_race <- as.data.frame(round(table(census$race)/ nrow(census) * 100, 2))
                ggplot(freq_race, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Race") + ggtitle("Race structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$marital <- renderPlot({
                freq_marital <- as.data.frame(round(table(census$marital_status)/ nrow(census) * 100, 2))
                ggplot(freq_marital, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Marital status") + ggtitle("Marital status") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$education <- renderPlot({
                freq_educ <- as.data.frame(round(table(census$education)/ nrow(census) * 100, 2))
                ggplot(freq_educ, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Education structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Education level"))
        })
        output$occupation <- renderPlot({
                freq_occup <- as.data.frame(round(table(census$occupation)/ nrow(census) * 100, 2))
                ggplot(freq_occup, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Occupation structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Occupation"))
        })
        output$country <- renderPlot({
                freq_country <- as.data.frame(round(table(census$native_country)/ nrow(census) * 100, 2), 
                                              stringsAsFactors = F)
                freq_country <- filter(freq_country, Freq > 1)
                freq_country$Freq[1] <- 100 - freq_country$Freq[2] - freq_country$Freq[3]
                freq_country$Var1[1] <- 'Others'
                ggplot(freq_country, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Native country") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Native country"))
        })
        
        output$age <- renderPlot({
                hist(census$age, breaks=25, col="lightblue", xlab="Age", ylab="Frequency", main = 'Age Distribution')
        })
        output$sex <- renderPlot({
                freq_sex <- as.data.frame(round(table(census$sex)/ nrow(census) * 100, 2))
                ggplot(freq_sex, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Sex") +
                        theme(axis.text.x=element_blank()) + theme_bw() +
                        geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                                      label = percent(Freq/100)), size=5) + 
                        ggtitle("Gender structure") + theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$race <- renderPlot({
                freq_race <- as.data.frame(round(table(census$race)/ nrow(census) * 100, 2))
                ggplot(freq_race, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Race") + ggtitle("Race structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$marital <- renderPlot({
                freq_marital <- as.data.frame(round(table(census$marital_status)/ nrow(census) * 100, 2))
                ggplot(freq_marital, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Marital status") + ggtitle("Marital status") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$education <- renderPlot({
                freq_educ <- as.data.frame(round(table(census$education)/ nrow(census) * 100, 2))
                ggplot(freq_educ, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Education structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Education level"))
        })
        output$occupation <- renderPlot({
                freq_occup <- as.data.frame(round(table(census$occupation)/ nrow(census) * 100, 2))
                ggplot(freq_occup, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Occupation structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Occupation"))
        })
        output$country <- renderPlot({
                freq_country <- as.data.frame(round(table(census$native_country)/ nrow(census) * 100, 2), 
                                              stringsAsFactors = F)
                freq_country <- filter(freq_country, Freq > 1)
                freq_country$Freq[1] <- 100 - freq_country$Freq[2] - freq_country$Freq[3]
                freq_country$Var1[1] <- 'Others'
                ggplot(freq_country, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Native country") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Native country"))
        })
        
        # LOW INCOME
        census2 <- census[census$income == '<=50K',]
        output$age_low <- renderPlot({
                hist(census2$age, breaks=25, col="lightblue", xlab="Age", ylab="Frequency", main = 'Age Distribution')
        })
        output$sex_low <- renderPlot({
                freq_sex <- as.data.frame(round(table(census2$sex)/ nrow(census2) * 100, 2))
                ggplot(freq_sex, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Sex") +
                        theme(axis.text.x=element_blank()) + theme_bw() +
                        geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                                      label = percent(Freq/100)), size=5) + 
                        ggtitle("Gender structure") + theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$race_low <- renderPlot({
                freq_race <- as.data.frame(round(table(census2$race)/ nrow(census2) * 100, 2))
                ggplot(freq_race, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Race") + ggtitle("Race structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$marital_low <- renderPlot({
                freq_marital <- as.data.frame(round(table(census2$marital_status)/ nrow(census2) * 100, 2))
                ggplot(freq_marital, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Marital status") + ggtitle("Marital status") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$education_low <- renderPlot({
                freq_educ <- as.data.frame(round(table(census2$education)/ nrow(census2) * 100, 2))
                ggplot(freq_educ, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Education structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Education level"))
        })
        output$occupation_low <- renderPlot({
                freq_occup <- as.data.frame(round(table(census2$occupation)/ nrow(census2) * 100, 2))
                ggplot(freq_occup, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Occupation structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Occupation"))
        })
        output$country_low <- renderPlot({
                freq_country <- as.data.frame(round(table(census2$native_country)/ nrow(census2) * 100, 2), 
                                              stringsAsFactors = F)
                freq_country <- filter(freq_country, Freq > 1)
                freq_country$Freq[1] <- 100 - freq_country$Freq[2] - freq_country$Freq[3]
                freq_country$Var1[1] <- 'Others'
                ggplot(freq_country, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Native country") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Native country"))
        })
        
        # HIGH INCOME
        census3 <- census[census$income == '>50K',]
        output$age_high <- renderPlot({
                hist(census3$age, breaks=25, col="lightblue", xlab="Age", ylab="Frequency", main = 'Age Distribution')
        })
        output$sex_high <- renderPlot({
                freq_sex <- as.data.frame(round(table(census3$sex)/ nrow(census3) * 100, 2))
                ggplot(freq_sex, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Sex") +
                        theme(axis.text.x=element_blank()) + theme_bw() +
                        geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                                      label = percent(Freq/100)), size=5) + 
                        ggtitle("Gender structure") + theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$race_high <- renderPlot({
                freq_race <- as.data.frame(round(table(census3$race)/ nrow(census3) * 100, 2))
                ggplot(freq_race, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Race") + ggtitle("Race structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$marital_high <- renderPlot({
                freq_marital <- as.data.frame(round(table(census3$marital_status)/ nrow(census3) * 100, 2))
                ggplot(freq_marital, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + scale_fill_brewer("Marital status") + ggtitle("Marital status") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  
        })
        output$education_high <- renderPlot({
                freq_educ <- as.data.frame(round(table(census3$education)/ nrow(census3) * 100, 2))
                ggplot(freq_educ, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Education structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Education level"))
        })
        output$occupation_high <- renderPlot({
                freq_occup <- as.data.frame(round(table(census3$occupation)/ nrow(census3) * 100, 2))
                ggplot(freq_occup, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Occupation structure") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Occupation"))
        })
        output$country_high <- renderPlot({
                freq_country <- as.data.frame(round(table(census3$native_country)/ nrow(census3) * 100, 2), 
                                              stringsAsFactors = F)
                freq_country <- filter(freq_country, Freq > 1)
                freq_country$Freq[1] <- 100 - freq_country$Freq[2] - freq_country$Freq[3]
                freq_country$Var1[1] <- 'Others'
                ggplot(freq_country, aes(x="", y=Freq, fill=Var1)) + geom_bar(width = 1, stat = "identity") + 
                        coord_polar("y", start=0) + ggtitle("Native country") + theme_bw() + 
                        theme(plot.title = element_text(lineheight=1, face="bold"))  + 
                        guides(fill=guide_legend(title="Native country"))
        })
        
        ########## INCOME  PREDICTION ###########
        output$unique_sex <- renderUI({
                sex <- levels(census$sex)
                selectInput('sex', 'Choose your sex:', sex)
        })
        # Random Forest
        set.seed(1234)
        census_sample <- census[sample(nrow(census), 10000), ]
        modfit <- train(income ~ ., data = census_sample, method = 'rpart')
        output$text1 <- renderText({
                test <- census[0:0,-9]
                #t <- c(42, 'Masters', 'Married-civ-spouse', 'Prof-specialty', 'White', 'Male', 40, 'United-States')
                test[1,] <- c(input$age, input$education, input$marital, input$occupation, input$race, 
                              input$sex, input$hours, input$country)
                test <- mutate(test, age = as.numeric(age), hours_per_week = as.numeric(hours_per_week))
                pred <- predict(modfit, test)
                paste('CART Algorithm predicts that your income in USA will be', as.character(pred), ' dollars over a year')
        })

})








# exploratory data analysis (Low income, high income, all income groups)



