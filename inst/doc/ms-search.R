## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new()

## ------------------------------------------------------------------------
header <- c('accession', 'formula',      'ms.mode', 'ms.level', 'peak.mz',  'peak.intensity', 'peak.relative.intensity', 'peak.formula',       'msprecannot',  'msprecmz', 'peak.attr')
db <- rbind.data.frame(                                                                                                                      
       list('BML80005',  'C12H14N2O2',   'pos',     1,          219.1127765, 373076,          999,                       NA_character_,        NA_character_, NA_real_,   NA_character_),
       list('BML80012',  'C15H18O8',     'pos',     1,          327.1074765, 33174,           999,                       NA_character_,        NA_character_, NA_real_,   NA_character_),
       list('BML80013',  'C15H18O8',     'neg',     1,          325.0929235, 1595373,         561,                       NA_character_,        NA_character_, NA_real_,   NA_character_),
       list('BML80013',  'C15H18O8',     'neg',     1,          361.069602,  2841342,         999,                       NA_character_,        NA_character_, NA_real_,   NA_character_),
       list('BML80020',  'C17H21NO3',    'pos',     1,          288.1593765, 781807,          999,                       NA_character_,        NA_character_, NA_real_,   NA_character_),
       list('BML80020',  'C17H21NO3',    'pos',     1,          310.141318,  11409,           15 ,                       NA_character_,        NA_character_, NA_real_,   NA_character_),
	   list('AU200951',  'C7H5F3O',      'neg',     2,          161.0238,    38176  ,         999,                      'C7H4F3O-',           '[M-H]-',       161.022 ,   '[M-H]-'     ),
	   list('AU200951',  'C7H5F3O',      'neg',     2,          162.0274,    1780   ,         46 ,                      'C6[13]CH4F3O-',      '[M-H]-',       161.022 ,   NA_character_),
	   list('AU200951',  'C7H5F3O',      'neg',     2,          141.0167,    616    ,         16 ,                      'C7H3F2O-',           '[M-H]-',       161.022 ,   NA_character_),
	   list('AU200952',  'C7H5F3O',      'neg',     2,          161.0246,    6180   ,         999,                      'C7H4F3O-',           '[M-H]-',       161.022 ,   '[M-H]-'     ),
	   list('AU200952',  'C7H5F3O',      'neg',     2,          141.0184,    1384   ,         223,                      'C7H3F2O-',           '[M-H]-',       161.022 ,   NA_character_),
	   list('AU200952',  'C7H5F3O',      'neg',     2,          121.0113,    1180   ,         190,                      'C7H2FO-',            '[M-H]-',       161.022 ,   NA_character_),
	   list('AU200952',  'C7H5F3O',      'neg',     2,          162.0282,    388    ,         62 ,                      'C6[13]CH4F3O-',      '[M-H]-',       161.022 ,   NA_character_),
	   list('AU200953',  'C7H5F3O',      'neg',     2,          121.0113,    828    ,         999,                      'C7H2FO-',            '[M-H]-',       161.022 ,   NA_character_),
	   list('AU200953',  'C7H5F3O',      'neg',     2,          141.0174,    300    ,         361,                      'C7H3F2O-',           '[M-H]-',       161.022 ,   NA_character_),
	   list('AU325851',  'C10H12N2O3S',  'neg',     2,          239.0502,    4580   ,         999,                      'C10H11N2O3S-',       '[M-H]-',       239.0496,   '[M-H]-'     ),
	   list('AU325851',  'C10H12N2O3S',  'neg',     2,          240.0525,    468    ,         102,                      'C9[13]CH11N2O3S-',   '[M-H]-',       239.0496,   NA_character_),
	   list('AU325851',  'C10H12N2O3S',  'neg',     2,          241.0471,    312    ,         68 ,                      'C10H11N2O3[34]S-',   '[M-H]-',       239.0496,   NA_character_),
	   list('AU341051',  'C9H10Cl2N2O',  'neg',     2,          231.0102,    30800  ,         999,                      'C9H9Cl2N2O- [M-H]-', '[M-H]-',       231.0097,   NA_character_),
	   list('AU341051',  'C9H10Cl2N2O',  'neg',     2,          233.0077,    13532  ,         438,                      'C9H9Cl[37]ClN2O-',   '[M-H]-',       231.0097,   NA_character_),
	   list('AU341051',  'C9H10Cl2N2O',  'neg',     2,          232.0129,    2024   ,         65 ,                      'C8[13]CH9Cl2N2O-',   '[M-H]-',       231.0097,   NA_character_),
	   list('AU341051',  'C9H10Cl2N2O',  'neg',     2,          185.9529,    1672   ,         54 ,                      'C7H2Cl2NO-',         '[M-H]-',       231.0097,   NA_character_),
	   list('AU341051',  'C9H10Cl2N2O',  'neg',     2,          187.9496,    868    ,         28 ,                      'C7H2Cl[37]ClNO-',    '[M-H]-',       231.0097,   NA_character_),
	   list('AU341051',  'C9H10Cl2N2O',  'neg',     2,          159.9737,    844    ,         27 ,                      'C6H4Cl2N-',          '[M-H]-',       231.0097,   NA_character_),
	   list('AU341051',  'C9H10Cl2N2O',  'neg',     2,          161.9711,    404    ,         13 ,                      'C6H4Cl[37]ClN-',     '[M-H]-',       231.0097,   NA_character_),
	   list('AU158001',  'C17H19NO3',    'pos',     2,          286.1456,    1073792,         999,                      'C17H20NO3+',         '[M+H]+',       286.1438,   '[M+H]+'     ),
	   list('AU158001',  'C17H19NO3',    'pos',     2,          287.1488,    157332 ,         146,                      'C16[13]CH20NO3+',    '[M+H]+',       286.1438,   NA_character_),
	   list('AU158001',  'C17H19NO3',    'pos',     2,          288.1514,    15604  ,         14 ,                      'C15[13]C2H20NO3+',   '[M+H]+',       286.1438,   NA_character_),
	   list('AU158002',  'C17H19NO3',    'pos',     2,          286.1457,    1338896,         999,                      'C17H20NO3+',         '[M+H]+',       286.1438,   '[M+H]+'     ),
	   list('AU158002',  'C17H19NO3',    'pos',     2,          287.1489,    227244 ,         169,                      'C16[13]CH20NO3+',    '[M+H]+',       286.1438,   NA_character_),
	   list('AU158002',  'C17H19NO3',    'pos',     2,          229.0869,    20980  ,         15 ,                      'C14H13O3+',          '[M+H]+',       286.1438,   NA_character_),
	   list('AU158002',  'C17H19NO3',    'pos',     2,          288.1513,    19640  ,         14 ,                      'C15[13]C2H20NO3+',   '[M+H]+',       286.1438,   NA_character_),
	   list('AU158002',  'C17H19NO3',    'pos',     2,          201.0918,    19520  ,         14 ,                      'C13H13O2+',          '[M+H]+',       286.1438,   NA_character_),
	   list('AU158002',  'C17H19NO3',    'pos',     2,          268.1343,    8808   ,         6  ,                      'C17H18NO2+',         '[M+H]+',       286.1438,   NA_character_),
	   list('AU158002',  'C17H19NO3',    'pos',     2,          211.076 ,    8660   ,         6  ,                      'C14H11O2+',          '[M+H]+',       286.1438,   NA_character_),
	   list('AU116602',  'C4H6N2S',      'pos',     2,          115.0334,    6556   ,         999,                      'C4H7N2S+',           '[M+H]+',       115.0324,   '[M+H]+'     ),
	   list('AU116606',  'C4H6N2S',      'pos',     2,          115.0334,    39940  ,         999,                      'C4H7N2S+',           '[M+H]+',       115.0324,   '[M+H]+'     ),
	   list('AU116606',  'C4H6N2S',      'pos',     2,          116.0365,    2808   ,         70 ,                      'C3[13]CH7N2S+',      '[M+H]+',       115.0324,   NA_character_),
	   list('AU116606',  'C4H6N2S',      'pos',     2,          117.0293,    2596   ,         64 ,                      'C4H7N2[34]S+',       '[M+H]+',       115.0324,   NA_character_),
            stringsAsFactors = FALSE)
names(db) <- header

## ------------------------------------------------------------------------
conn <- mybiodb$getFactory()$createConn('mass.csv.file')
conn$setDb(db)
conn$setField('peak.mztheo', 'peak.mz')

## ------------------------------------------------------------------------
conn$getMzValues(max.results = 10)

## ------------------------------------------------------------------------
conn$getMzValues(max.results = 10, ms.mode = 'pos')

## ------------------------------------------------------------------------
conn$getMzValues(max.results = 10, precursor = TRUE)

## ------------------------------------------------------------------------
conn$getMzValues(max.results = 10, ms.level = 2)

## ------------------------------------------------------------------------
conn$searchMzRange(mz.min = 115, mz.max = 115.1, max.results = 5)

## ------------------------------------------------------------------------
conn$searchMzTol(mz = 115, mz.tol = 0.1, mz.tol.unit = 'plain', max.results = 5)

## ------------------------------------------------------------------------
# Define spectrum to match:
spectrum <- data.frame(mz = c(286.1456, 287.1488, 288.1514), rel.int = c(999, 158, 18))

# Search for match:
conn$msmsSearch(spectrum, precursor.mz = 286.1438, mz.tol = 0.1, mz.tol.unit = 'plain', ms.mode = 'pos')

