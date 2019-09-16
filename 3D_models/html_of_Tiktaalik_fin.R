# This script makes an html file with the 3D files of the pectoral fin of Tiktaalik.
# OBJ files were saved from Amira and then reduced with Meshlab (Filters> Remeshing, Simplification and Reconstruction > Simplification: Quadric edge collapse decimation; use “0.9” in the percentage reduction box).
#
# September 2019
# TAS 


# initialize packages ####
# install.packages("devtools")
library(devtools)
# install_github('aaronolsen/svgViewR')
library(svgViewR)


# Make a new svg file
svg.new('Tiktaalik_pectoral_fin.html', 
        window.title='TiktaalikPectFin3D',
        col="gray8",
        mode='webgl',
        panel=TRUE)

# load the 3D OBJ files ####
# humerus
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/humerus.obj', name='01 - humerus', col='gainsboro')
# radius
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/radius.obj', name='02 - radius', col='gainsboro')
# ulna
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/ulna.obj', name='03 - ulna', col='gainsboro')
# intermedium
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/intermedium.obj', name='04 - intermedium', col='gainsboro')
# ulnare
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/ulnare.obj', name='05 - ulnare', col='gainsboro')
# mesomere 3
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/mesomere3.obj', name='06 - mesomere 3', col='gainsboro')
# mesomere 4
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/mesomere4.obj', name='07 - mesomere 4', col='gainsboro')
# mesomere 5
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/mesomere5.obj', name='08 - mesomere 5', col='gainsboro')
# radial 3
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/radial3.obj', name='09 - radial 3', col='gainsboro')
# radial 4
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/radial4.obj', name='10 - radial 4', col='gainsboro')
# radial 5
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/radial5.obj', name='11 - radial 5', col='gainsboro')
# dermal rays
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/DLeps.obj', name='12 - dorsal rays', col='orange1')
# ventral rays
svg_mesh <- svg.mesh('Documents/papers/submitted/tetrapodomorph_dermal_ray/stewart_tetrapodomorph_fin_project/OBJ_format/meshlab_reduced/VLeps.obj', name='13 - ventral rays', col='steelblue1')

# close the svg file
svg.close()

