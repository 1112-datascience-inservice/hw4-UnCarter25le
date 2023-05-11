# Interactive web service of PCA and CA analysis by Shinyapp

#### Name: [楊昇豐_Carter]
#### Student ID: [111971013]
#### ShinyApps link: [<https://uncarter25le.shinyapps.io/NCCU_DS2023_hw4_111971013/>]


## My Claim

1. Libraries I use:

      ```
    library(dplyr)    
    library("ggplot2")
    library(shiny)
    library(ggbiplot)
    library(FactoMineR) 
    library(factoextra)
    library(corrplot)
    library(DT)
    library(MASS)

    ```

2. The tour guide of my design:

    1. Part1: This is for controlling `the elements of rendered plot` on the aspect of size, such as picutre, label, data point, and legend.

    2. Part2: This is for choosing which `main content` to show. According to requirements of assignment, main content includes PCA, CA, and explacation for Iris Dataset.

    3. Part3: This is affected by the `clicking tabs` at the Part2.

    4. Part4: This will exist if main content needs `sub content` to explain further.

    5. Part5: This is for controlling `the numbers of data input and which PC element` to render.
      
        - The `horizontal scroll` bar will affect PCA, CA and Iris Dataset.

        - The `select option` will affect PCA ggbiplot.

        - The rest of Part5 is `for check now` what the digits controlled at this are.

    6. Part6: If this assignment does touch you, you are encouraged to type words in the `text input window` and copy the `text ouput` to send me email.

    ![layout_design](/demo_photos/layout_design.png)



3. Annoying Problem I confront but get around in the end:

    1. deploying error:
        - shinyapp can't fetch the MASS package at the version, 7.3-58.4, My local project defines.

    2. The method really does the trick:
        
        - Based on [MASS package info from CRAN](https://cran.r-project.org/web/packages/MASS/index.html), all I need to do `is  install MASS package at the version 7.3-60 locally`. And, Shinyapp server can fetch the correct version of MASS when bundling my project. 

          ![deploy_error](/demo_photos/deploy_error.png)

        - Neitzen's advice isn't working, which is executing `devtools::install_version("MASS", "7.3-51.1")`. The command will prompt me to install another program, Rstool accounting for 3GB, and it doesn' do the trick! 

          ![neitzen_advice_not_work](/demo_photos/neitzen_advice_not_work.png)



---

## Description

<p align="center">
 <img src="/images/PCA.png" width="48%" height="48%" >
 <img src="/images/CA.png" width="48%" height="48%" >
<p/>


- Make the interactive web service of PCA and CA analysis by Shinyapp
- You might start by integrating the following example (pcr.R) into shiny app. Of course, feel free to find other appropriate packages for PCA.

### pca.R

```R
data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
```
ggbiplot is not on CRAN, it is only available on GitHub. It can be installed by following cmmand.
```
install.packages('remotes', dependencies = TRUE)
remotes::install_github("vqv/ggbiplot")
```

### How to publish your work on shinyapps.io? 🤔
At the beginning, you have to sign up for [shinyapps.io](https://www.shinyapps.io/) account first
 
In RStudio IDE, you can manage your shinyapps.io accounts by going to `Tools → Global Options → Publishing`. 

<p align="center">
 <img src="/images/shinyapp_on_rstudio.png" width="70%" height="70%" >
<p/>

After connecting your account you can start making your shinyapp. Please comply with the following folder structure. where `110753xxx`need to change to your student number.
```
110753xxx
   |-- app.R
```
After finishing your shinyapps code, you can preview your website in the console with the following command.  
```
library(shiny)
runApp("110753xxx")
```
Then, you can see the following screen, and there is a `publish` button in the upper right corner, click it to upload the website.
Please note that you need to make sure your title conforms to the format → `NCCU_DS2023_hw4_studendID`

<p align="center">
 <img src="/images/shinyapp_on_rstudio_2.png" width="60%" height="60%" >
<p/>

## Scores: By Peer Evaluation
### Base Task (80 pts)
- [ ] [20 pts] Basic information: `name`、`department`、`student number`
- [ ] [30 pts] Make a shiny interactive web site to show PCA analysis (as the following picture) for the iris data such that users can specify which component to show (i.e., PC1, PC2, PC3 ...)
- [ ] [30 pts] Make a shiny interactive web site to show correspondence analysis (CA) analysis for the iris data

### Subjective (20 pts)
- [ ] [5 pts] Aesthetic 🌷
- [ ] [5 pts] Interactivity 🖥️
- [ ] [5 pts] Content rich 📖
- [ ] [max 5pts, each for 2 pts] Extra visualizations or tables to show more information 
  * input data
  * PCA result (i.e., amount of variances ... )
  * ...

### Penalty: -5 points of each classmate works 🙅‍♂️ 
Points will be deducted if you do not help grade other students' work!!

## Notes
* Please use R version 4
* This assignment does not accept make up. 👀
* About Peer Evaluation
  * Each student's work will be evaluated by other classmates (randomly selected 6-8), and the final score of the project will be determined by taking the average of the remaining scores. 
  * Each student is also required to evaluate other people's work, and if they fail to evaluate one, points will be deducted.
  * Peer Evaluation will start within one week after the assignment is due. We will be notified to students through mail.
* About ShinyApps
  * Please share your shinyapp link & student ID on top of Readme.md
  * You must publish your work on [shinyapps.io](https://www.shinyapps.io/)，so that you can get the public link. Please make sure `your link is available`, and title conforms to the format → `NCCU_DS2023_hw4_studendID`
  * Please comply with the following folder structure.  where `110753xxx`need to change to your student number.
   ```
  110753xxx
     |-- app.R
   ```

## Example
#### https://changlab.shinyapps.io/ggvisExample/
#### https://smalleyes.shinyapps.io/NCCU_DS2023_hw4_110753202/
