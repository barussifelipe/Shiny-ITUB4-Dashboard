source("global.R")
connection = dbConnect(RPostgres::Postgres(), 
                       dbname = 'financial', 
                       host = 'postgres', 
                       port = 5432, 
                       user = 'postgres', 
                       password = '8273')

client_data = dbGetQuery(connection, "SELECT * FROM public.data1")
source("ui.R")
source("server.R")
shinyApp(ui, server)