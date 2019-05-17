library(brapi)
con = brapi::ba_db()$musabase

femaleList = as.character(unique(bananadata$Mother))[-c(1,6, 19,20, 23)]
femaleList = femaleList[femaleList>0]
acclist = list()

for(female in femaleList){
  print(female)
  acc = ba_germplasm_search(con, germplasmName = female)[,c('germplasmDbId','germplasmName')]
  acclist[[female]] <- acc
}



maleList = as.character(unique(bananadata$Father))[-5]
maleList = maleList[maleList>0]

for(male in maleList){
  print(male)
  acc = ba_germplasm_search(con, germplasmName = male)[,c('germplasmDbId','germplasmName')]
  acclist[[male]] <- acc
 }


df = do.call(rbind,acclist)

df$link = paste0("https://musabase.org/stock/", df$germplasmDbId,"/view")

# manually add links for accessions with spaces
df2 = data.frame(germplasmName = c("ITC1455-Mchare Mlelembo","Mchare Laini", "ITC0249-Calcutta 4" ,"Kisukari Mchare", "02145/1320"),
           link = c("https://musabase.org/stock/65887/view",
                    "https://musabase.org/stock/65045/view",
                    "https://musabase.org/stock/123380/view",
                    "https://musabase.org/stock/65048/view",
                    "https://musabase.org/stock/65419/view"
                    )
)

accessions_links = plyr::rbind.fill(df, df2)[-1]

accessions_links$link = paste0("<a href='",accessions_links_in_musabase$link,"'>",accessions_links$germplasmName,"</a>")
mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))


fwrite(accessions_links, file = '../data/accessions_links_in_musabase.csv', row.names = F)


