FROM rocker/tidyverse

RUN install2.r --error 

RUN rm -rf /tmp/downloaded_packages /tmp/*.rds
