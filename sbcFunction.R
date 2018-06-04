sbcFunctionUI <- function(id){
  tabItem(
    tabName = "relatieFamilie",
    bootstrapPage(
      mainPanel(width = 12,
                column(
                  width = 2,
                  textInput(
                    inputId = "prima",
                    label = "Introduceti Prima Persoana",
                    value = ""
                  )
                ),

                column(
                  width = 2,
                  radioButtons(inputId = "sexPrima",
                               label =  "Sex",
                               choices = c("F" = "F",
                                           "M" = "M"),
                               selected = "F")
                ),
                column(
                  width = 2,
                  textInput(
                    inputId = "a2a",
                    label = "Introduceti Cealalta Persoana",
                    value = ""
                  )
                ),

                column(
                  width = 2,
                  radioButtons(inputId = "sexa2a",
                               label =  "Sex",
                               choices = c("F" = "F",
                                           "M" = "M"),
                               selected = "F")
                ),
                column(
                  width = 2,
                  radioButtons(inputId = "buton",
                               label =  "Prima persoana  pentru a2a este:",
                               choices = c("Bunica/Bunic" = "bunica",
                                           "Matusa/Unchi" = "matusa",
                                           "Frate/Sora" = "frate",
                                           "Mama/Tata" = "mama"),
                               selected = "bunica")
                ),
                column(
                  width = 4,
                  textOutput("selected_var"))

      )
    )
  )
}

sbcFunctionServer <- function(input = input, output = output, session = session){

  doc <- htmlTreeParse(file = paste0("../sbc.xml"),useInternalNodes = TRUE)

  fapte = xpathApply(doc, "//fapte")
  reguli = xpathApply(doc, "//reguli")
  parinti = xpathApply(doc, "//parinte")
  # nrParinti<- length(xmlChildren(fapte[[1]]))

  parinteDataFrame<- xmlValue(xmlChildren(parinti[[1]])[[1]])
 # parinte <- as.data.frame(numeParinte)

  numeParinte <- c()
  sexParinte <- c()
  nrCopii <- c()
  numeCopii <- c()
  sexCopii <- c()
  numeCopii <- c()

  for(i in 1:length(parinti)){
    numeParinte[i] <- xmlValue(xmlChildren(parinti[[i]])[[1]])
    sexParinte[i] <- xmlValue(xmlChildren(parinti[[i]])[[2]])
    nrCopii[i] <-  length(getNodeSet(parinti[[i]], "copil"))
    numeCopii[i] <- ""
    sexCopii[i] <- ""

    for(j in 1:nrCopii[i]){
      if(j != nrCopii[i]){
        numeCopii[i] <- paste0(numeCopii[i], xmlValue(getNodeSet(getNodeSet(parinti[[i]],"copil")[[j]],"nume")[[1]]),",")
        sexCopii[i] <- paste0(sexCopii[i],"", xmlValue(getNodeSet(getNodeSet(parinti[[i]],"copil")[[j]],"sex")[[1]]),",")
      }else{
        numeCopii[i] <- paste0(numeCopii[i], xmlValue(getNodeSet(getNodeSet(parinti[[i]],"copil")[[j]],"nume")[[1]]))
        sexCopii[i] <- paste0(sexCopii[i],"", xmlValue(getNodeSet(getNodeSet(parinti[[i]],"copil")[[j]],"sex")[[1]]))
      }
    }
  }

  parinte <- as.data.frame(numeParinte)
  parinte <- cbind(parinte,sexParinte,nrCopii,numeCopii,sexCopii)

  parinte$numeCopii<- str_split_fixed(parinte$numeCopii, ",", 3)
  dataNume<-as.data.frame(parinte$numeCopii)
  parinte$sexCopii<- str_split_fixed(parinte$sexCopii, ",", 3)
  dataSexe<-as.data.frame(parinte$sexCopii)
  parinte<-parinte[,1:3]
  parinte <- cbind(parinte,dataNume,dataSexe)
  colnames(parinte) <- c("numeParinte","sexParinte","nrCopii","copil1","copil2","copil3","sex1","sex2","sex3")


  dataDefault <- reactive({
 if( as.character(parinte[parinte$numeParinte == input$prima,]$sexParinte) == input$sexPrima){
   parintelea2a <- as.character(parinte[((parinte$copil1 == input$a2a) & (parinte$sex1 == input$sexa2a)) |
                                          ((parinte$copil2 == input$a2a) & (parinte$sex2 == input$sexa2a)) |
                                          ((parinte$copil3 == input$a2a) & (parinte$sex3 == input$sexa2a))
                                        ,]$numeParinte)

 }
    parintelea2a
  })


  output$selected_var <- renderText({
     if((input$prima != "") & (input$a2a != "")){

       if(input$buton == "bunica"){
         if(((as.character(parinte[parinte$numeParinte == input$prima,]$copil1 ) %in% dataDefault() )|
                                       (as.character(parinte[parinte$numeParinte == input$prima,]$copil2 ) %in% dataDefault() )|
                                       (as.character(parinte[parinte$numeParinte == input$prima,]$copil3 ) %in% dataDefault() ))){
           if(input$sexPrima == "F"){
             paste0(input$prima," este bunica lui ", input$a2a)
           }else{
             paste0(input$prima," este bunicul lui ", input$a2a)
           }
         }else{
             if(input$sexPrima == "F"){
               paste0(input$prima," Nu este bunica lui ", input$a2a)
             }else{
               paste0(input$prima," Nu este bunicul lui ", input$a2a)
             }
         }
       }

       else {
         if(input$buton == "matusa"){

           parintiPrimaPers <- as.character(parinte[(parinte$copil1 == input$prima) | (parinte$copil2 ==  input$prima)|(parinte$copil3 == input$prima) ,]$numeParinte)

           parintia2aPers <- as.character(dataDefault())
           primulBunic <- as.character(parinte[(parinte$copil1 == parintia2aPers[1]) | (parinte$copil2 ==  parintia2aPers[1])|(parinte$copil3 == parintia2aPers[1]) ,]$numeParinte)[1]
           al2leaBunic <- as.character(parinte[(parinte$copil1 == parintia2aPers[1]) | (parinte$copil2 ==  parintia2aPers[1])|(parinte$copil3 == parintia2aPers[1]) ,]$numeParinte)[2]
           al3leaBunic <- as.character(parinte[(parinte$copil1 == parintia2aPers[2]) | (parinte$copil2 ==  parintia2aPers[2])|(parinte$copil3 == parintia2aPers[2]) ,]$numeParinte)[1]
           al4leaBunic <- as.character(parinte[(parinte$copil1 == parintia2aPers[2]) | (parinte$copil2 ==  parintia2aPers[2])|(parinte$copil3 == parintia2aPers[2]) ,]$numeParinte)[2]

           if(primulBunic %in%parintiPrimaPers |
              al2leaBunic %in%parintiPrimaPers |
              al3leaBunic %in%parintiPrimaPers |
              al4leaBunic %in%parintiPrimaPers
           ){

             if(input$sexPrima == "F"){
               paste0(input$prima," este matusa lui ", input$a2a)
             }else{
               paste0(input$prima," este unchiul lui ", input$a2a)
             }
           }else{
             if(input$sexPrima == "F"){
               paste0(input$prima," Nu este matusa lui ", input$a2a)
             }else{
               paste0(input$prima," Nu este unchiul lui ", input$a2a)
             }
           }
         }else{
           if(input$buton == "frate"){

             parintia2aPers <- as.character(dataDefault())
             parintiPrimaPers <- as.character(parinte[(parinte$copil1 == input$prima) | (parinte$copil2 ==  input$prima)|(parinte$copil3 == input$prima) ,]$numeParinte)

            print(parintia2aPers)
            print(parintiPrimaPers)
               if(parintia2aPers %in% parintiPrimaPers){
                 if(input$sexPrima == "F"){
                   paste0(input$prima," este sora lui ", input$a2a)
                 }else{
                   paste0(input$prima," este fratele lui ", input$a2a)
                 }
               }

             else{
               if(input$sexPrima == "F"){
                 paste0(input$prima," Nu este sora lui ", input$a2a)
               }else{
                 paste0(input$prima," Nu este fratele lui ", input$a2a)
               }
             }


           }else{
             if(input$buton == "mama"){

               parintia2aPers <- as.character(dataDefault())
              print(paste0("parintia2a:", parintia2aPers))
              print(paste0("prima:", input$prima))
               if(input$prima %in% parintia2aPers){
                 if(input$sexPrima == "F"){
                   paste0(input$prima," este mama lui ", input$a2a)
                 }else{
                   paste0(input$prima," este tatal lui ", input$a2a)
                 }
               }else{
                 if(input$sexPrima == "F"){
                   paste0(input$prima," Nu este mama lui ", input$a2a)
                 }else{
                   paste0(input$prima," Nu este tatal lui ", input$a2a)
                 }
               }


             }
         }






       }
       }
  }})
}

