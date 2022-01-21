#!/bin/bash


# Shows the use of variables
MyVar='some string'
echo 'the current value of the variables is' $MyVar
echo 'Please entre a new string'
read MyVar
echo 'the current value of variable is' $MyVar

## Reading multile values
echo 'Write two numbers separated by space(s)'
read a b
echo 'you entered' $a 'and' $b '. Their sum is:'
mysum=`expr $a + $b` # be carful with spaces in code
echo $mysum

