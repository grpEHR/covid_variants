# COVID-19 variants plot

Please download the following data and place it in the directory `raw`:
- [Wellcome Sanger Insitute COVID–19 Genomic Surveillance](https://covid19.sanger.ac.uk/lineages/raw?lineageView=1&lineages=B.1.1.7%2CB.1.617.2%2CB.1.1.529&colours=1%2C6%2C2)
- [GOV.UK Coronavirus (COVID-19) in the UK](https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&format=csv)

Please run the script `code/process_data.R` followed by `code/plot_data.R`.

Intermediate files will be saved in the directory `data`.

The COVID-19 variants plot will be saved in the directory `output`.
