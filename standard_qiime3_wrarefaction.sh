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
echo $2
echo $3
echo $4
echo $5
echo $7

## just E, B, and ED overtime
qiime feature-table filter-samples \
--i-table table-$tablename"".qza \
--m-metadata-file $mapper \
--p-where "$6" \
--o-filtered-table table_$name"".qza

# make taxonomic assignments
#qiime feature-classifier classify-sklearn \
#--i-classifier gg-13-8-99-515-806-nb-classifier.qza \
#--i-reads filtered_rep-seqs.qza \
#--o-classification taxonomy.qza

# filter out wolbachia reads
#qiime taxa filter-table --i-table table_$name"".qza --i-taxonomy filtered_taxonomy.qza --p-exclude Wolbachia --o-filtered-table table_$name""_noW.qza

for i in $(seq 1 100);
do
qiime diversity core-metrics-phylogenetic \
--quiet \
--i-phylogeny rooted-tree-$treename"".qza \
--i-table table_$name"".qza \
--p-sampling-depth $sdepth \
--m-metadata-file $mapper \
--output-dir core-metrics-results-$name""-$i

cd core-metrics-results-$name""-$i
qiime tools export \
--input-path rarefied_table.qza \
--output-path rarefied_table/
biom convert -i rarefied_table/feature-table.biom -o rarefied_table.txt --to-tsv
qiime tools export \
--input-path weighted_unifrac_distance_matrix.qza \
--output-path weighted_unifrac_distance_matrix/
qiime tools export \
--input-path unweighted_unifrac_distance_matrix.qza \
--output-path unweighted_unifrac_distance_matrix/
#qiime tools export \
#--input-path unweighted_unifrac_pcoa_results.qza \
#--output-path unweighted_unifrac_pcoa_results/
#qiime tools export \
#--input-path weighted_unifrac_pcoa_results.qza \
#--output-path weighted_unifrac_pcoa_results/
#qiime tools export \
#--input-path bray_curtis_pcoa_results.qza \
#--output-path bray_curtis_pcoa_results/
qiime tools export \
--input-path bray_curtis_distance_matrix.qza \
--output-path bray_curtis_distance_matrix/
qiime tools export \
--input-path shannon_vector.qza \
--output-path shannon_vector/
qiime tools export \
--input-path evenness_vector.qza \
--output-path evenness_vector/
qiime tools export \
--input-path observed_features_vector.qza \
--output-path observed_features_vector/
qiime tools export \
--input-path faith_pd_vector.qza \
--output-path faith_pd_vector/
cd ..

done

cp table_$name"".qza core-metrics-results-$name""/.
rm table_$name"".qza

## run the alpha rarefaction
if [ $7 = yes ]
then
qiime diversity alpha-rarefaction \
--i-table core-metrics-results-$name""/table_$name"".qza \
--i-phylogeny rooted-tree-$treename"".qza \
--p-max-depth $sdepth \
--m-metadata-file $mapper \
--o-visualization core-metrics-results-$name""/alpha-rarefaction-$name"".qzv

qiime feature-table summarize \
--i-table core-metrics-results-$name""/table_$name"".qza \
--o-visualization core-metrics-results-$name""/table_$name"".qzv \
--m-sample-metadata-file $mapper

fi
