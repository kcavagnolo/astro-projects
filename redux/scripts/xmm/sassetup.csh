#installation for sas
setenv SAS_DIR /data/rft/sas/xmmsas_20040318_1831
setenv SAS_PATH /data/rft/sas/xmmsas_20040318_1831
source $SAS_DIR/sas-setup.csh
setenv SAS_CCFPATH /data/rft/sas/ccf
setenv SAS_CCFFILES /data/rft/sas/ccf
setenv SAS_ODF /data/rft/legacy/0020_2542/ODF
#echo 'running cifbuild'
#cifbuild -V 2
setenv SAS_CCF ccf.cif
#echo 'running odfingest'
#odfingest -V 2
setenv SAS_ODF `ls -1 *SUM.SAS`
