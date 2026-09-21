#!/bin/sh

#  standard_qiime2.sh
#
#
#  Created by John Chaston on 7/25/18.
#

## taxonomy folder, tree.qza, table.qza, mapper.tsv
# $7 if yes makes the rarefaction curve and a table.qzv of the rarefaction. $7 if no or if not entered skips these steps, both of which are fairly computationally intensive, so it can save time to ignore them.

tablename=$1
treename=$2
name=$3
sdepth=$4
mapper=$5
iterations=$7
echo $2
echo $3
echo $4
echo $5
echo $6
echo $7
echo $8


qiime feature-table filter-samples \
  --i-table table-$tablename"".qza \
  --m-metadata-file $mapper \
  --p-where "$6" \
  --o-filtered-table table_unrarefied_betarare-$name.qza
 
qiime diversity beta-rarefaction \
  --i-table table_unrarefied_betarare-$name.qza \
  --p-metric braycurtis \
  --p-clustering-method upgma \
  --p-sampling-depth $sdepth \
  --p-iterations $iterations \
  --p-correlation-method spearman \
  --m-metadata-file $mapper \
  --o-visualization core-metrics-results-$name/beta-rarefaction_parent_braycurtis_""$sdepth""_$iterations""it.qzv

qiime diversity beta-rarefaction \
  --i-table table_unrarefied_betarare-$name.qza \
  --i-phylogeny rooted-tree-$treename"".qza \
  --p-metric weighted_unifrac \
  --p-clustering-method upgma \
  --p-sampling-depth $sdepth \
  --p-iterations $iterations \
  --p-correlation-method spearman \
  --m-metadata-file $mapper \
  --o-visualization core-metrics-results-$name/beta-rarefaction_parent_weightedUnifrac_""$sdepth""_$iterations""it.qzv

qiime diversity beta-rarefaction \
  --i-table table_unrarefied_betarare-$name.qza \
  --i-phylogeny rooted-tree-$treename"".qza \
  --p-metric unweighted_unifrac \
  --p-clustering-method upgma \
  --p-sampling-depth $sdepth \
  --p-iterations $iterations \
  --p-correlation-method spearman \
  --m-metadata-file $mapper \
  --o-visualization core-metrics-results-$name/beta-rarefaction_parent_unweightedUnifrac_""$sdepth""_$iterations""it.qzv
