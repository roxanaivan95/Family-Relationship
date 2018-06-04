server <- shinyServer(function(input, output, session) {
  sbcFunctionServer(input = input, output = output, session = session)
})
