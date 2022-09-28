## Graph colours

# Country
outcome.colours = c("#ef7c10","#dc251f", "#009ee2", "#00722a", "#9364cc")
names(outcome.colours) = c("Obesity", "Hypertension", "Diabetes", "Lipid disorder", "Metabolic syndrome")

# Outcome
country.colours = c("#0019a8","#dc251f")
names(country.colours) = c("Belgium","Luxembourg")
gender.colours = c("#0019a8","#dc251f")
names(gender.colours) = c("Male","Female")

# Chemical family
annot = readRDS("../Data/Compound_family_annotation.rds")
annot.colours = colorspace::lighten(colorRampPalette(RColorBrewer::brewer.pal(n=12,name='Paired'))(length(unique(annot))), amount=0.2)
names(annot.colours) = unique(annot)