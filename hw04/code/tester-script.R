# ===================================================================
# Title:Function Tests for HW04
# Description:
#   This script test functions created for HW04
# Input(s): R script "functions.R"; R script "tests.R"
# Output(s): md file 'test-reporter.txt'
# Author: Josia Yuan
# Date: 11-11-2017
# ===================================================================

# test script
library(testthat)
# source in functions to be tested
source('functions.R')
sink('../output/test-reporter.txt')
test_file('tests.R')
sink()
