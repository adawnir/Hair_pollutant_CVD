## Graph colours

# Country
country.colours = c("forestgreen","royalblue","tomato")
names(country.colours) = c("Belgium","France","Luxembourg")

# Chemical family
annot = readRDS("../Data/Compound_family_annotation.rds")
annot.colours = colorspace::lighten(colorRampPalette(RColorBrewer::brewer.pal(n=12,name='Paired'))(length(unique(annot))), amount=0.2)
names(annot.colours) = unique(annot)