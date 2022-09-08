## Graph colours

# Country
outcome.colours = c("orange","lightcoral", "skyblue", "limegreen", "purple")
names(outcome.colours) = c("Obesity", "Hypertension", "Diabetes", "Lipid disorder", "Metabolic syndrome")

# Outcome
country.colours = c("royalblue","tomato")
names(country.colours) = c("Belgium","Luxembourg")

# Chemical family
annot = readRDS("../Data/Compound_family_annotation.rds")
annot.colours = colorspace::lighten(colorRampPalette(RColorBrewer::brewer.pal(n=12,name='Paired'))(length(unique(annot))), amount=0.2)
names(annot.colours) = unique(annot)