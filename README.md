Biodb
=====

[![Build Status](https://travis-ci.org/pkrog/biodb.svg?branch=master)](https://travis-ci.org/pkrog/biodb)

An R package for connecting to chemical and biological databases.

## Databases connection status

In this table, you will find a list of all online databases accessible through biodb, with a status. The status can  avrry, depending on changes in the database API that can break the biodb library. When this happens, the status column is marked with a red cross, and an issue is opened and listed inside column *Related issue*.

Database               | Key                 | Status | Online tests | Related issue                                     | Related PR | Explanations
------------           | ----------          | :---:  | :----------: | ------------------------------------------------- | ---------- | ---------------------
ChEBI                  | chebi               |   ✅   |
ChemSpider             | chemspider          |   ✅   |     off      |                                                   |            | Online tests are not run because ChemSpider free plan has a limit of 1000 requests per month.
ExPASy ENZYME          | expasy.enzyme       |   ✅   |
HMDB Metabolites       | hmdb.metabolites    |   ✅   |              |                                                   |            |               
KEGG Compound          | kegg.compound       |   ✅   |
KEGG Enzyme            | kegg.enzyme         |   ✅   |
KEGG Genes             | kegg.genes          |   ✅   |
KEGG Module            | kegg.module         |   ✅   |
KEGG Pathway           | kegg.pathway        |   ✅   |
KEGG Reaction          | kegg.reaction       |   ✅   |
LIPID MAPS Structure   | lipidmaps.structure |   ✅   |
Massbank               | massbank            |   ❌   |              | [#30](https://github.com/pkrog/biodb/issues/30)   |            | The API is being migrated to a new version (not yet available). In the meantime the curent API is broken and not maintained.
miRBase Mature         | mirbase.mature      |   ✅   |     off      |                                                   |            | Online tests are off because miRBase Mature needs download of the whole database through FTP, and FTP is not possible from inside Travis-CI.
NCBI CCDS              | ncbi.ccds           |   ✅   |
NCBI Gene              | ncbi.gene           |   ✅   |
NCBI PubChem Compound  | ncbi.pubchem.comp   |   ✅   |              |                                                   |            | 
NCBI PubChem Substance | ncbi.pubchem.subst  |   ✅   |
NCI CACTUS             | nci.cactus          |   ✅   | 
PeakForest Compound    | peakforest.compound |   ✅   |              |                                                   |            | <https://metabohub.peakforest.org/> is down.
PeakForest Mass        | peakforest.mass     |   ✅   |              |                                                   |            | <https://metabohub.peakforest.org/> is down.
UniProt                | uniprot             |   ✅   |              |                                                   |            | 

**Legend**:
 * ✅ Database is working fine with Biodb connector.
 * ❌ Biodb database connector is broken. This means that Travis-CI tests are disabled for this connector, until a fix is implemented. See "Related issue", "Related PR" and "Explanations" columns in table, for more details.

## Introduction

With *biodb* you can:

 * Access entries by accession number and let *biodb* download them for you.
 * Take advantage of the cache system, that saves the results of all sent requests for you. If you send again the same request, the cached result will be used instead of contacting the database. The cache system can be disabled.
 * Download whole databases and access entries by accession number locally.
 * Rely on *biodb* to access correctly the database, respecting the published access policy (i.e.: not sending too much requests). *biodb* uses a special class for scheduling requests on each database.
 * Switch from one database to another easily (providing they offer the same type of information), not changing a line in your code. This is because entries are populated with values found from the database, using always the same keys.
 * Search for MS spectra by peaks in Massbank, Peakforest and in-house database.
 * Search for MSMS spectra.
 * Export any database into a CSV file.

Version 1.0 of the library gives access to the following databases:

 * ChEBI.
 * ChemSpider.
 * ExPASy ENZYME.
 * HMDB Metabolites.
 * KEGG Compound.
 * LIPID MAPS Structure.
 * Mass CSV File (an in-house database stored inside a CSV file).
 * MassBank.
 * miRBase Mature.
 * NCBI CCDS.
 * NCBI Gene.
 * PubChem Compound.
 * PubChem Substance.
 * PeakForest Mass.
 * PeakForest Compound.
 * UniProt.

Here are some of the fields accessible through the retrieved entries:

 * Chemical formula.
 * InChI.
 * InChI Key.
 * SMILES.
 * Common names and IUPAC names.
 * Charge.
 * Average mass.
 * Monoisotopic mass.
 * Molecular mass.
 * Sequence.
 * MS device.
 * MS Level.
 * MS mode.
 * MS precursor M/Z.
 * MS precursor annotation.
 * Peak table (containing M/Z, intensity, relative intensity, attribution, composition).
 * Chromatographic column name.
 * Chromatographic column length.
 * Chromatographic column diameter.
 * Chromatographic solvent.
 * Chromatographic retention time.
 * Chromatographic retention time unit.

## Installation

**biodb** is part of [bioconda](https://github.com/orgs/bioconda/dashboard), so you can install it using conda. This means also that is possible to install it automatically in Galaxy, for a tool, if the conda system is enabled.

**biodb** is also installable directly from GitHub, or from a local repository. See below for details.

### Install from GitHub

Using package `devtools` you can install Biodb directly from GitHub:
```r
devtools::install_github('pkrog/biodb', dependencies = TRUE, build_vignettes = FALSE)
```

### Install from local repository

First, you need to clone the GitHub repos:
```bash
git clone -b master https://github.com/pkrog/biodb.git
```

Then you install *biodb* using the `devtools` package:
```r
devtools::install_local('/your/path/to/biodb', dependencies = TRUE, build_vignettes = FALSE)
```

## Documentation

Documentation is available on the [Wiki](https://github.com/pkrog/biodb/wiki) page. You will find there all the R vignettes, but without the output of code snippets.

Once in R, you can get documentation about main classes with the standard help function:
```r
?biodb::Biodb
```
Some of the classes you can get help about are: Biodb, BiodbFactory, BiodbConfig, BiodbCache, BiodbDbsInfo, BiodbEntryFields, BiodbObserver, BiodbConn, BiodbEntry, MassdbConn, RemotedbConn.

Several vignettes are also available. To get a list of them run:
```r
vignette(package = 'biodb')
```

To open a vignette in a browser, use its name:
```r
vignette('init', package = 'biodb')
```

## Contributing

If you wish to contribute to the *biodb* package, you first need to create an account under GitHub. You can then either to ask to become a contributor or fork the project and submit a merge request.

Debugging, enhancement or creation of a database connector or an entry parser are of course most welcome.

### Running tests

Under UNIX and UNIX like systems (macOS, Linux, ...) you can run `make test` to run the tests. You will need to have R accessible from command line, and also to have installed the R package `testthat`.

The plain command `make test` will run the offline tests, which uses cache files recorded inside this repository under `tests/res/offline-cache`. All databases will be tested.

If you wish to test only some databases, you can specify them inside the environment variable `DATABASES`:
```bash
DATABASES=massbank,chebi,mirbase.mature make test
```

If you want to run online tests, use the environment variable `MODES` to specify it:
```bash
MODES=online make test
```

The value `quick.online` for `MODES` turns off download of whole databases if they have already been downloaded and are stored inside the cache system.
The value `all` for `MODES` run tests in all modes: `online`, `quick.online` and `offline`.

The two environment variables can be combined together.

## Changelog

### Version 1.2.2

 * Accept a data frame as input in searchMsPeaks(). (#287)
 * Fix issue with NA values in input in searchMsPeaks(). (#286)

### Version 1.2.1

No change, use a new version number in order to avoid confusion in conda.

### Version 1.2.0

 * Update list of MassBank prefixes and fix deep link url. (#271)
 * Integrate compound information into Peakforest mass search result. (#279)
 * Create method collapseResultsDataFrame(). (#278)
 * Correct search with an N/A value for M/Z. (#277)
 * Disable tests on NCBI PubChem comp. (#276)
 * Check for error between proxy and server.
 * Remove urltools dependency. (#273)
 * Correct PeakForest MZ search with multiple ranges. (#270)
 * Correct Peakforest compound search compound failure. (#268)
 * Correct PeakForest Mass RT test. (#267)
 * Introduce exclusion of tests on Travis-CI for out of order databases. (#266)
 * Allow writing of MassCsvFile database. (#256)
 * Now there is only one single URL scheduler. (#253)
 * Allow multiple connector instantiations of the same database class. (#248)
 * Use GitHub as source for Massbank data. (#243)
 * Correct URL for KEGG Compound entry page. (#242)
 * lipidmaps.structure test ref entries update. (#240)
 * Check mass.field in searchCompound(). (#239)
 * Update HMDB0000001 entry for testing. (#238)

### Version 1.1.3

 * Allow setting of new names for MS mode. (#236)

### Version 1.1.2

 * Modify parameters of searchMsPeaks().

### Version 1.1.1

 * Fix RT matching. (#235)

### Version 1.1.0

 * Improve error message on SVN failure. (#226)
 * Update uniprot test files. (#227)
 * Update HMDB test ref files. (#225)
 * Correct convertion of RT input into seconds.
 * Test and correct RT match.
 * Correct PeakForest chrom col list.
 * No comment chars while parsing entry CSV file.
 * Correct test of cache system activation in URL scheduler.
 * Correct parsing of peaks table in MassCsvFile.
 * Add MS peak fields definitions.
 * Implement RT tol exp. (#221)
 * Handle msprecmz field in mass.csv.file. (#219)
 * Handle RT unit. (#217)
 * Hotfix/peakforest new url. (#215)
 * Create LICENSE.

### Version 1.0.2

 * Hotfix/cpp build. (#224)

### Version 1.0.1

 * Hotfix/peakforest new url. (#215)

### Version 1.0

 * Split Travis-CI tests for databases.
 * Always allow download of Mirbase.
 * Hotfix/massbank relative intensity (#205)
 * Test presence of MS level field in Massbank entry.
 * Implement listKeys() method in config class.
 * Reduce size of vignettes HTML files.
 * Update email in package description.
 * Correct regexp in Rbuildignore.
 * Correct Lipidmaps parsing of synonyms.
 * Add missing entry fields for MassCsvFile.
 * Correct max.results param in getEntryIds().
 * Implement web service LMSDRecord  in Lipidmaps.
 * Implement searchCompound() for Mirbase.
 * Implement ws.search for PeakForest.
 * Implement searchCompound() for NcbiGene.
 * Implement searchCompound() for Pubchem Comp.
 * Update test ref files of several databases.
 * Update expasy enzyme URL.
 * Implement getEntryImageUrl() for all databases.
 * Close connections to log files.
 * Implement missing getEntryPageUrl() methods.
 * Merge name and fullnames in Uniprot.
 * Implement getChromCol() for Massbank.
 * Speed up msmsSearch() test.
 * Set database from data frame in MassCsvFile.
 * Forbid vector for enumerated value.
 * Correct msmsSearch when no IDs are found.
 * Implement enumerated type for entry fields.
 * Correct handle of NAs in CSV entry,
 * Implement searchMsPeaks.
 * Write missing show() methods.
 * Speed up Peakforest MSMS test.
 * Implement enzyme-search-de web service.

### Version 0.9

An alpha version.

 * Create token vignette.
 * Update lipidmaps.structure test ref files.
 * Update enzyme test ref files.
 * ChEBI searchCompound corrections.
 * Add new examples to README.
 * Add descriptions to all fields.
 * Improve massbank examples.
 * Improve CAUTION message displayed when encountering a cache file with bad encoding.
 * Ignore CSV file output when running examples.
 * Exclude DEBUG messages by default in Logger class.
 * Remove failure when SVN default path isn't found.
 * Add class diagram pictures.
 * Complete searchCompound() example.
 * Add link detail for KEGG web services doc.
 * Implement searchCompound() for KEGG Compound.
 * Write and test new web services for exact mass and molecular mass for Kegg Compound.
 * KeggConn::ws.find works.
 * Add method for erasing whole cache or cache subfolder.
 * Implement searchCompound in ChemSpider.
 * Implement and test ChemSpider web service for searching by mass.
 * Implement searchCompounds() for Uniprot.
 * Encode URL before sending request.
 * Implement getEntryIds() for Uniprot.
 * Write Uniprot query method.
 * Replace all ref test tables by JSON files, for all tested databases.
 * Add license.
 * Decrease memory usage of HMDB metabolite zip extraction by not using XML library.
 * Move all URLs info to DbsInfo.
 * Correct XML namespace in getLiteEntity ChEBI web service. Solves #91.
 * Update test reference files for ChEBI.
 * Replace SOAP call by REST for getLiteEntity service in ChEBI.
 * Add debug messages for POST messages.
 * Move content-type into DbInfo class.
 * Move scheduler parameters into BiodbDbsInfo.
 * Increase Pubchem scheduler frequency to 5 request per second.
 * Declare abstract classes.

### Version 0.6

A beta version.

## Citations

### ChEBI

<http://www.ebi.ac.uk/chebi/>

 * Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B., Kale, N., Muthukrishnan, V., Owen, G., Turner, S., Williams, M., and Steinbeck, C. (2013) The ChEBI reference database and ontology for biologically relevant chemistry: enhancements for 2013. Nucleic Acids Res, <http://dx.doi.org/10.1093/nar/gks1146>.

### ChemSpider

<http://www.chemspider.com>

 * Harry E. Pence and Antony Williams. ChemSpider: An Online Chemical Information Resource. Journal of Chemical Education 2010 87 (11), 1123-1124, <http://dx.doi.org/10.1021/ed100697w>.

### Expasy Enzyme

<https://enzyme.expasy.org>

 * Bairoch A. The ENZYME database in 2000. Nucleic Acids Res 28:304-305(2000), <https://enzyme.expasy.org/data/enz00.pdf>.

### HMDB

<http://www.hmdb.ca>

 * Wishart DS, Tzur D, Knox C, et al., HMDB: the Human Metabolome Database. Nucleic Acids Res. 2007 Jan;35(Database issue):D521-6, <https://doi.org/10.1093/nar/gkl923>.
 * Wishart DS, Knox C, Guo AC, et al., HMDB: a knowledgebase for the human metabolome. Nucleic Acids Res. 2009 37(Database issue):D603-610, <https://doi.org/10.1093/nar/gkn810>.
 * Wishart DS, Jewison T, Guo AC, Wilson M, Knox C, et al., HMDB 3.0 — The Human Metabolome Database in 2013. Nucleic Acids Res. 2013. Jan 1;41(D1):D801-7, <https://doi.org/10.1093/nar/gks1065>.

### KEGG

<http://www.kegg.jp>

 * Kanehisa, M.; Toward pathway engineering: a new database of genetic and molecular pathways. Science & Technology Japan, No. 59, pp. 34-38 (1996), <http://www.kanehisa.jp/docs/archive/stj.pdf>.
 * Kanehisa, M.; A database for post-genome analysis. Trends Genet. 13, 375-376 (1997), <https://doi.org/10.1016/S0168-9525(97)01223-7>.
 * Kanehisa, M. and Goto, S.; KEGG: Kyoto Encyclopedia of Genes and Genomes. Nucleic Acids Res. 28, 27-30 (2000), <https://doi.org/10.1093/nar/28.1.27>.
 * Kanehisa, M. and Goto, S.; KEGG: Kyoto Encyclopedia of Genes and Genomes. Nucleic Acids Res. 28, 27-30 (2000), <https://doi.org/10.1093/nar/28.1.27>.
 * Kanehisa, M., Goto, S., Kawashima, S., and Nakaya, A.; The KEGG databases at GenomeNet. Nucleic Acids Res. 30, 42-46 (2002), <https://doi.org/10.1093/nar/30.1.42>.
 * Kanehisa, M., Goto, S., Kawashima, S., Okuno, Y., and Hattori, M.; The KEGG resources for deciphering the genome. Nucleic Acids Res. 32, D277-D280 (2004), <https://doi.org/10.1093/nar/gkh063>.
 * Kanehisa, M., Goto, S., Hattori, M., Aoki-Kinoshita, K.F., Itoh, M., Kawashima, S., Katayama, T., Araki, M., and Hirakawa, M.; From genomics to chemical genomics: new developments in KEGG. Nucleic Acids Res. 34, D354-357 (2006), <https://doi.org/10.1093/nar/gkj102>.
 * Kanehisa, M., Araki, M., Goto, S., Hattori, M., Hirakawa, M., Itoh, M., Katayama, T., Kawashima, S., Okuda, S., Tokimatsu, T., and Yamanishi, Y.; KEGG for linking genomes to life and the environment. Nucleic Acids Res. 36, D480-D484 (2008), <https://doi.org/10.1093/nar/gkm882>.
 * Kanehisa, M., Goto, S., Furumichi, M., Tanabe, M., and Hirakawa, M.; KEGG for representation and analysis of molecular networks involving diseases and drugs. Nucleic Acids Res. 38, D355-D360 (2010), <https://doi.org/10.1093/nar/gkp896>.
 * Kanehisa, M., Goto, S., Sato, Y., Furumichi, M., and Tanabe, M.; KEGG for integration and interpretation of large-scale molecular datasets. Nucleic Acids Res. 40, D109-D114 (2012), <https://doi.org/10.1093/nar/gkr988>.
 * Kanehisa, M., Goto, S., Sato, Y., Kawashima, M., Furumichi, M., and Tanabe, M.; Data, information, knowledge and principle: back to metabolism in KEGG. Nucleic Acids Res. 42, D199–D205 (2014), <https://doi.org/10.1093/nar/gkt1076>.
 * Kanehisa, M., Sato, Y., Kawashima, M., Furumichi, M., and Tanabe, M.; KEGG as a reference resource for gene and protein annotation. Nucleic Acids Res. 44, D457-D462 (2016), <https://doi.org/10.1093/nar/gkv1070>.
 * Kanehisa, Furumichi, M., Tanabe, M., Sato, Y., and Morishima, K.; KEGG: new perspectives on genomes, pathways, diseases and drugs. Nucleic Acids Res. 45, D353-D361 (2017), <https://doi.org/10.1093/nar/gkw1092>.

### Lipidmaps Structure

<http://www.lipidmaps.org>

 * Sud M., Fahy E., Cotter D., Brown A., Dennis E., Glass C., Murphy R., Raetz C., Russell D., and Subramaniam S. LMSD: LIPID MAPS structure database. Nucleic Acids Research 35, D527-32 (2006), <https://doi.org/10.1093/nar/gkl838>.

### Massbank

<http://www.massbank.eu>

 * Hisayuki Horai, Masanori Arita, Shigehiko Kanaya, Yoshito Nihei, Tasuku Ikeda, Kazuhiro Suwa, Yuya Ojima, Kenichi Tanaka, Satoshi Tanaka, Ken Aoshima, Yoshiya Oda, Yuji Kakazu, Miyako Kusano, Takayuki Tohge, Fumio Matsuda, Yuji Sawada, Masami Yokota Hirai, Hiroki Nakanishi, Kazutaka Ikeda, Naoshige Akimoto, Takashi Maoka, Hiroki Takahashi, Takeshi Ara, Nozomu Sakurai, Hideyuki Suzuki, Daisuke Shibata, Steffen Neumann, Takashi Iida, Ken Tanaka, Kimito Funatsu, Fumito Matsuura, Tomoyoshi Soga, Ryo Taguchi, Kazuki Saito, Takaaki Nishioka. 2010. MassBank: a public repository for sharing mass spectral data for life sciences. Journal of Mass Spectrometry, <http://dx.doi.org/10.1002/jms.1777>.

### miRBase

<http://mirbase.org>

 * Ana Kozomara, Sam Griffiths-Jones. (2013) miRBase: annotating high confidence microRNAs using deep sequencing data. Nucleic Acids Research, Volume 42, Issue D1, 1 January 2014, Pages D68–D73, <https://doi.org/10.1093/nar/gkt1181>.
 * Ana Kozomara, Sam Griffiths-Jones. (2010) miRBase: integrating microRNA annotation and deep-sequencing data. Nucleic Acids Research, Volume 39, Issue suppl_1, 1 January 2011, Pages D152–D157, <https://doi.org/10.1093/nar/gkq1027>.
 * Sam Griffiths-Jones, Harpreet Kaur Saini, Stijn van Dongen, Anton J. Enright. (2007) miRBase: tools for microRNA genomics. Nucleic Acids Research, Volume 36, Issue suppl_1, 1 January 2008, Pages D154–D158, <https://doi.org/10.1093/nar/gkm952>.
 * Sam Griffiths-Jones, Russell J. Grocock, Stijn van Dongen, Alex Bateman, Anton J. Enright. (2006) miRBase: microRNA sequences, targets and gene nomenclature. Nucleic Acids Research, Volume 34, Issue suppl_1, 1 January 2006, Pages D140–D144, <https://doi.org/10.1093/nar/gkj112>.

### NCBI Gene
 
<https://www.ncbi.nlm.nih.gov/gene>

 * Gene [Internet]. Bethesda (MD): National Library of Medicine (US), National Center for Biotechnology Information; 2004 – [cited 2018 Feb 08]. Available from: <https://www.ncbi.nlm.nih.gov/gene/>.

### NCBI CCDS

 <https://www.ncbi.nlm.nih.gov/projects/CCDS/CcdsBrowse.cgi>

 * Pruitt KD, Harrow J, Harte RA, Wallin C, Diekhans M, Maglott DR, Searle S, Farrell CM, Loveland JE, Ruef BJ, Hart E, Suner MM, Landrum MJ, Aken B, Ayling S, Baertsch R, Fernandez-Banet J, Cherry JL, Curwen V, Dicuccio M, Kellis M, Lee J, Lin MF, Schuster M, Shkeda A, Amid C, Brown G, Dukhanina O, Frankish A, Hart J, Maidak BL, Mudge J, Murphy MR, Murphy T, Rajan J, Rajput B, Riddick LD, Snow C, Steward C, Webb D, Weber JA, Wilming L, Wu W, Birney E, Haussler D, Hubbard T, Ostell J, Durbin R, Lipman D. (2009) The consensus coding sequence (CCDS) project: Identifying a common protein-coding gene set for the human and mouse genomes. Genome Res. 2009 Jul;19(7):1316-23, <https://doi.org/10.1101/gr.080531.108>.
 * Harte RA, Farrell CM, Loveland JE, Suner MM, Wilming L, Aken B, Barrell D, Frankish A, Wallin C, Searle S, Diekhans M, Harrow J, Pruitt KD. (2012) Tracking and coordinating an international curation effort for the CCDS Project. Database 2012 Mar 20;2012:bas008. doi: 10.1093/database/bas008, <https://doi.org/10.1093/database/bas008>.
 * Farrell CM, O'Leary NA, Harte RA, Loveland JE, Wilming LG, Wallin C, Diekhans M, Barrell D, Searle SM, Aken B, Hiatt SM, Frankish A, Suner MM, Rajput B, Steward CA, Brown GR, Bennett R, Murphy M, Wu W, Kay MP, Hart J, Rajan J, Weber J, Snow C, Riddick LD, Hunt T, Webb D, Thomas M, Tamez P, Rangwala SH, McGarvey KM, Pujar S, Shkeda A, Mudge JM, Gonzalez JM, Gilbert JG, Trevanion SJ, Baertsch R, Harrow JL, Hubbard T, Ostell JM, Haussler D, Pruitt KD. (2014) Current status and new features of the Consensus Coding Sequence database. Nucleic Acids Res. 2014 Jan 1;42(1):D865-72, <https://doi.org/10.1093/nar/gkt1059>.

### NCBI Pubchem

<https://pubchem.ncbi.nlm.nih.gov>

 * Kim S, Thiessen PA, Bolton EE, Chen J, Fu G, Gindulyte A, Han L, He J, He S, Shoemaker BA, Wang J, Yu B, Zhang J, Bryant SH. PubChem Substance and Compound databases. Nucleic Acids Res. 2016 Jan 4; 44(D1):D1202-13. Epub 2015 Sep 22, <https://doi.org/10.1093/nar/gkv951>.

### Peakforest

<https://peakforest.org/>

 * PeakForest [Internet], a spectral data portal for Metabolomics community - storing, curating and annotation services for metabolic profiles of biological matrix. INRA / MetaboHUB ; 2017 – [cited 2018 Feb 08]. Available from: <https://peakforest.org/>.

### Uniprot

<http://www.uniprot.org>

 * The UniProt Consortium. UniProt: the universal protein knowledgebase. Nucleic Acids Res. 45: D158-D169 (2017), <https://doi.org/10.1093/nar/gkw1099>.
 * Pundir S., Martin M.J., O’Donovan C. (2017) UniProt Protein Knowledgebase. In: Wu C., Arighi C., Ross K. (eds) Protein Bioinformatics. Methods in Molecular Biology, vol 1558. Humana Press, New York, NY. <https://doi.org/10.1007/978-1-4939-6783-4_2>.
