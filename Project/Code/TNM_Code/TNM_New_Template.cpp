#include <iostream>
#include <cmath>
#include <random>
#include<fstream>
#include<algorithm>
#include <fstream>
#include <experimental/filesystem>
#include <sys/stat.h> // mkdir
#include <dirent.h>
#include <thread>
#include <chrono>

using namespace std;

////////////////////////////
// User-Defined Variables //
////////////////////////////

// These variables can be easily changed to alter the output of the model

string dir = "../../Results/TNM_Output/";    // Directory for output of model
int fragmentation = 0;                      // If 0 creates new community in non-fragmented landscape, if 1 loads in community and loads in your own landscape from command line argument 4.
                                            // but this can be changed in the command line argument, as argument 3.

int defSeed = 1;                            // This is the default seed that will be used if one is not provided in the command line argument (recommend using command line

const int cellRows = 11;                    //Sets the number of cells in the rows of the landscape (Note: must match landscape file used in code, if using file).
const int cellCols = 11;                    // Sets the number of cells in the columns of the landscape (Note: must match landscape file used in code).
const int numCells = cellRows * cellCols;   // Sets the number of cells in the landscape (this needs to be created outside of this code, likely in R, with appropriate distances etc).
int landscapeArray[cellCols][cellRows];     // Landscape array for filling with default values, or reading in from landscape created in .txt format.
int landscapeCoords[numCells][2];           // Coordinates (x,y) of each cell in the landscape for use in distance calculations.
double distArray[numCells][numCells];       // Distance from each cell to every other cell, for use in dispersal and interactions.
int Rfr = 10;                               // Set carrying capacity which will be the same for all cells.

const int numSpec = 95;                    // Number of species in the model (each species will have interacitons, specialism, dispersal etc associated with it).
const int numGenPre = 500;                 // Number of generations to run the model pre fragmentation. Each generation is broken into time steps.
const int numGenPost = 2000;                //2000 Number of generations to run the model after fragmentation. Each generation is broken into time steps.
const int initPop = 100;                    // Number of individuals to put into each cell at the start of the model.

const float probDeath = 0.15;               // Probability of individual dying if chosen.
float probImm = 0.0005;                      // Probability of an individual immigrating into a cell (individual of a random species suddenly occurring in a cell).
float probImmFrag = 0;                  // Probability of an individual immigrating into a cell after fragmentation (individual of a random species suddenly occurring in a cell).
float probDisp = 0.01;                       // Probability of an individual dispersing from one cell to another cell. This is a baseline, and will increase from this with increasing density.
const float probInt = 0.5;                  // Fraction of non-zero interactions. Aka approximate fraction of interactions that will occur.

int weightInt = 5;                         // Weighting of importance of interactions. Higher value puts more importance on interactions in calculating probability of offspring.
int weightSpec = 2;                         // Weighting of importance of specialism.

///////////////////////
// Define Variables //
//////////////////////

// These variables should not be changed unless the model itself is being edited

int totalGen = numGenPre + numGenPost;      // Total generations the model will run for.
double J[numSpec][numSpec];                 // J-matrix which includes interactions between all species, with number of rows and cols equal to number of species. 
double SvG[numSpec];                        // Stores specialism value for each species.
double disp[numSpec];                       // Stores dispersal ability for each species.
int totalPop = 0;                           // Stores the total population across all cells in the model at a given generation.
int cellPop[numCells];                      // Stores the total population of in each cell at a given generation.
int totalPopSpec[numSpec];                      // Stores the total population of each species at a given generation.
int cellPopSpec[numCells][numSpec];             // Stores the total population of each species in each cell at a given generation.
int totalRich = 0;                          // Stores the total species richness of the model at a given generation.
int cellRich[numCells];                     // Stores the species richness of a specific cell at a given generation.
int cellList[numCells][2];                     // Lists numbers of cell (e,g with 6 cells it would read, 0,1,2,3,4,5), and whether they are matrix or forest (0 = matrix, 1 = forest).
int cellOrder[numCells];                        // Array to store the randomly selected order of cells for dynamics.
double distMatrix[numCells][numCells];          // Stores distances between all cells in the landscape.
double probDispDen = 0;                         // Store density dependent probability of diserpsal
int seed;



double percPop[numSpec][2];                     // Store percentage of population each species makes up
int stabilityArray[1000][numSpec];
vector <int> mainPop;
vector <int> prevPop = {0};
int stableGens = 0;

int tmax = 50;                              // Number of times each dynamic could happen per cell per generation (e.g probDeath could happen 50 times per cell per gen).

static const double two_pi  = 2.0*3.141592653;

//////////////////////////
// Initialise functions //
//////////////////////////

void createJMatrix(double (&J)[numSpec][numSpec], double probInt, mt19937& eng);
void createSvG(double (&SvG)[numSpec], mt19937& eng);
void createDisp(double (&disp)[numSpec]);
double uniform(mt19937& eng);
double gaussian(mt19937& eng);
void initialisePop(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells],  int (&cellRich)[numCells], 
    int &totalRich, int numSpec, int numCells, mt19937& eng);
int chooseInRange(int a, int b, mt19937& eng);
void immigration(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells],  int (&cellRich)[numCells], int &totalRich ,
double prob, int cell, int numSpec, mt19937& eng);
void kill(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells],  int (&cellRich)[numCells], int &totalRich,
double prob, int cell, int numSpec, mt19937& eng);
void reproduction(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells], 
int (&cellList)[numCells][2], double (&J)[numSpec][numSpec], double (&SvG)[numSpec] , int cell, int numSpec, int Rfr, mt19937& eng);
double calculateInteractions(double (&J)[numSpec][numSpec], int (&cellPopSpec)[numCells][numSpec], int (&cellPop)[numCells], double (&distArray)[numCells][numCells],
    int cell, int numSpec, int ind);
void shuffle(int arr[], int arrElements, mt19937& eng);
int randomInd(int (&cellPopSpec)[numCells][numSpec], int (&cellPop)[numCells], int numSpec, int cell, mt19937& eng);
double specialism(int (&cellList)[numCells][2], double (&SvG)[numSpec], int cell, int Ind);
void cellCoords(int (&landscapeCoords)[numCells][2], int cols, int rows, int numCells);
void getDistances(double (&distArray)[numCells][numCells], int landscapeCoords[][2], int cellRows, int cellCols);
void dispersal(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells],  int (&cellRich)[numCells], int &totalRich,
double (&distArray)[numCells][numCells], double (&disp)[numSpec], double prob, int cell, int numSpec, mt19937& eng); 
vector<int> findValidCells(double (&distArray)[numCells][numCells], double distance, int cell);
double dispersalProb(int cellPop[numCells],int Rfr, int cell, double probDeath, double probDisp);
void fillCellList(int landscapeArray[][cellRows], int cellList[][2], int cellCols, int cellRows);

void store2ColFiles(ofstream &stream, int firstCol, int secondCol);
void store3ColFiles(ofstream &stream, int firstCol, int secondCol, int thirdCol[]);
void store4ColFiles(ofstream &stream, int firstCol, int secondCol, int thirdCol, int fourthCol[][numSpec]);
void storeNum(int num, string fileName, string outpath);
void readNum(int &num, string fileName, string inpath);

void calculatePercPop(double (&percPop)[numSpec][2], int (&totalPopSpec)[numSpec], int totalPop);
void sortPop(double (&percPop)[numSpec][2], int numSpec);
double findMax(double percPop[numSpec][2], int low, int high);
vector <int> findMainPops(double (&percPop)[numSpec][2]);
void keep_Stable_Species(int &totalPop, int &totalRich, int (&totalPopSpec)[numSpec], int (&cellPop)[numCells], int (&cellRich)[numCells], 
    int (&cellPopSpec)[numCells][numSpec], vector <int> mainPop, int numCells, int numSpec);

///////////////////////////////
//          Templates        //
///////////////////////////////

template <typename type>
void printArray(type arr[], int arrElements) {
    for (int i = 0; i < arrElements; i++){
        cout << arr[i] << " " << endl;
    }
    
}

template <typename type, int rows>
void print2DArray(type arr[][rows], int c, int r) {

    for (int i = 0; i < c; i++){
        for (int j = 0; j < r; j++){
            cout << arr[i][j] << " ";
        }
    cout << endl;
    }
}

template <typename type>
void storeArray(type arr[], int arrElements, string fileName, string outpath) {

    ofstream array;
    array.open(outpath + fileName);

    for (int i = 0; i < arrElements; i++){
        array << arr[i] << endl;
    }

    array.close();
    
}

template <typename type, int second>
void store2DArray(type arr[][second], int c, int r, string fileName, string outpath) {

    ofstream array;
    array.open(outpath + fileName);

    for (int i = 0; i < r; i++){
        for (int j = 0; j < c; j++){
            array << arr[i][j] << " ";
        }
        array << endl;
    }

    array.close();
    
}

template <typename type>
void readArray(type arr[], int arrElements, string fileName, string inpath) {

    ifstream array(inpath + fileName);

    for (int i = 0; i < arrElements; i++){
        array >> arr[i];
    }

}

template <typename type, int rows>
void read2DArray(type arr[][rows], int c, int r, string fileName, string inpath) {

    ifstream array(inpath + fileName);

    for (int i = 0; i < r; i++){
        for (int j = 0; j < c; j++){
            array >> arr[i][j];
        }
    }
}

template <                                                  //timer template FC
    class result_t   = std::chrono::seconds,
    class clock_t    = std::chrono::steady_clock,
    class duration_t = std::chrono::seconds
>
auto since(std::chrono::time_point<clock_t, duration_t> const& start)
{
    return std::chrono::duration_cast<result_t>(clock_t::now() - start);
}

///////////////////////////////
// Main Tangled Nature Model //
///////////////////////////////


int main(int argc, char *argv[]) {
    auto start = std::chrono::steady_clock::now(); // start timer FC
    
    //std::this_thread::sleep_for(10ms);
    
    //start = std::chrono::steady_clock::now();
    //std::this_thread::sleep_for(1ms);
 

    string fraginpath, fragName;
    int newSeed;

    /////////////
    // Check command Line
    /////////////

    // We only want to run under two conditions
    // 1) With just the seed defined, which means we are making a new community
    // 2) With the old community seed defined, a new seed, fragmentation (set to 1), and a landscape file to fragment to
    // Under all other circumstances we want to exit the program (other than no arguments, when we just make a new community with seed 1 as an example)

    if(argc == 2) {seed = atof(argv[1]);} else {seed = defSeed;}

    if(argc == 5) {
        seed = atof(argv[1]);
        newSeed = atof(argv[2]);
        fragmentation = atof(argv[3]);
        fragName = argv[4];
        fraginpath = "../../Data/Fragments/";
    }

    if(argc == 3 || argc == 4 || argc > 5) {
        cout << "Unexpected number of command line arguments" << endl;
        cout << "Either input a seed for new community" << endl;
        cout << "Or input the old seed, new seed, fragmentation (1), and a fragmented landscape file path" << endl;
        exit(0);
    }

    // Set working directories based on the seed taken from the command line argument
    string outpath = dir + "Seed_" + to_string(seed);       // Folder that outputs are saved to.
    string respath = outpath + "/Results";                  // Folder for output of results.
    string landpath = outpath + "/Landscape";                // Folder for output of landscape files.
    string finpath = outpath + "/Final_Output";              // Folder for final generation outputs for use in future models.
    string fragpath = outpath + "/Fragmentation";               // Folder to hold future fragmentation outcomes

    // Make new directory at output location named after the seed

    DIR* checkDir = opendir(outpath.c_str());
    if (checkDir && fragmentation == 0) {
        closedir(checkDir);
        cout << "Directory for seed " << seed << " already exists, exiting....." << endl;
        exit(0);
    } else if(checkDir && fragmentation == 1) {
        closedir(checkDir);
        cout << "Community exists, fragmenting...." << endl;
    } else if (!checkDir && fragmentation == 0){
        mkdir(outpath.c_str(),07777);
        cout << "Directory does not exist, creating... at " << outpath << endl;
    } else if(!checkDir && fragmentation == 1) {
        cout << "Directory doesnt exist, so no community to load....exiting" << endl;
        exit(0);
    }

    // Add more directories. 
    // One to store the final outputs for use in future models, another for landscape files, and another for the full results.
    if(fragmentation == 0) {
        mkdir(respath.c_str(), 07777);
        mkdir(finpath.c_str(), 07777);
        mkdir(landpath.c_str(), 07777);
    }
    // If fragmentation is occuring make a main fragmentation folder if it doesn't already exist
    if(fragmentation == 1) mkdir(fragpath.c_str(), 07777);
    // Then make a folder within that for the specific seed used for this fragmentation
    string fragseedpath = fragpath + "/Seed_" + to_string(newSeed);
    if(fragmentation == 1) mkdir(fragseedpath.c_str(), 07777);
    // Then add a folder within the fragmentaiton folder for this specific landscape structure, which will be named after the landscape
    string fragoutpath = fragseedpath + "/" + fragName;
    if(fragmentation == 1) mkdir(fragoutpath.c_str(), 07777);

    // Fill cellList with number of each cell (0,1,2,3 etc)
    // second column in cellList currently blank as it could change depending on the landscape (1 = forest, 0 = matrix)
    for (int i = 0; i < numCells; i++) {cellList[i][0] = i; cellOrder[i] = i;}

    if(fragmentation == 1) {seed = newSeed;}

    mt19937 eng(seed); // Seed random numbers

    // Check if we are loading a previous community to undergo fragmentation
    // or if we need to create a new community
    if(fragmentation == 1) {

        read2DArray<double, numSpec>(J, numSpec, numSpec, "/JMatrix.txt", outpath);
        readArray<double>(SvG, numSpec, "/SvG.txt", outpath);
        readArray<double>(disp, numSpec, "/disp.txt", outpath);

        // Read in predefined landscape with 1s for forest cells and 0 for matrix cells
        read2DArray<int, cellRows>(landscapeArray, cellCols, cellRows, fragName, fraginpath);
        fillCellList(landscapeArray, cellList, cellCols, cellRows);
        store2DArray<int, 2>(cellList, 2, numCells, "/cellList.txt", fragoutpath);
        store2DArray<int, cellRows>(landscapeArray, cellCols, cellRows, "/landscape.txt", fragoutpath);

        // Get coordinate for each cell in the landscape for distance calculation
        cellCoords(landscapeCoords, cellCols, cellRows, numCells);

        // Calculate distances between all cells
        getDistances(distArray, landscapeCoords, cellRows, cellCols);

        // store2DArray<double, numCells>(distArray, numCells, numCells, "/distArray_frag.txt", fragpath);

        readNum(totalPop, "/final_totalPop.txt", finpath);
        readNum(totalRich, "/final_totalRich.txt", finpath);
        readArray<int>(cellPop, numCells, "/final_cellPop.txt", finpath);
        readArray<int>(cellRich, numCells, "/final_cellRich.txt", finpath);
        readArray<int>(totalPopSpec, numSpec, "/final_totalPopSpec.txt", finpath);
        read2DArray<int, numSpec>(cellPopSpec, numSpec, numCells, "/final_cellPopSpec.txt", finpath);
        
       
    } else {

        // Create and store species traits
        createJMatrix(J, probInt, eng);
        store2DArray<double, numSpec>(J, numSpec, numSpec, "/JMatrix.txt", outpath);
        createSvG(SvG, eng);
        storeArray<double>(SvG, numSpec, "/SvG.txt", outpath);
        createDisp(disp);
        storeArray<double>(disp, numSpec, "/disp.txt", outpath);

        // Make default landscape based on rows, columns and numCells provided (aka all cells forest)
        for (int i = 0; i < cellRows; i++) {fill_n(landscapeArray[i], cellCols, 1);}
        for (int i = 0; i < numCells; i++) {cellList[i][1] = 1;}
        store2DArray<int, cellRows>(landscapeArray, cellCols, cellRows, "/landscape.txt", landpath);
        store2DArray<int, 2>(cellList, 2, numCells, "/cellList.txt", landpath);

        // Get coordinates for each cell for use in calculating distance
        cellCoords(landscapeCoords, cellCols, cellRows, numCells);

        // Calculate distances between all cells
        getDistances(distArray, landscapeCoords, cellRows, cellCols);
        store2DArray<double, numCells>(distArray, numCells, numCells, "/distArray.txt", landpath);

        // Set all cells to have no individuals in them
        fill_n(totalPopSpec, numSpec, 0);
        fill_n(cellRich, numCells, 0);
        fill_n(cellPop, numCells, 0);
        for (int i = 0; i < numCells; i++) {fill_n(cellPopSpec[i], numSpec, 0);}

        // Initialise population and store the initial population to file
        initialisePop(totalPopSpec, cellPopSpec, totalPop, cellPop, cellRich, totalRich, numSpec, numCells, eng);
        store2DArray<int, numSpec>(cellPopSpec, numSpec, numCells, "/initial_Pop.txt", outpath);

    }

    // Open files to output to
    if (fragmentation == 1) {
        respath = fragoutpath + "/Results";
        mkdir(respath.c_str(), 07777);     
    }

    ofstream s_totalPop;
    s_totalPop.open(respath + "/totalPop.txt");
    ofstream s_totalRich;
    s_totalRich.open(respath + "/totalRich.txt");
    ofstream s_cellRich;
    s_cellRich.open(respath + "/cellRich.txt");
    ofstream s_cellPop;
    s_cellPop.open(respath + "/cellPop.txt");
    ofstream s_totalPopSpec;
    s_totalPopSpec.open(respath + "/totalPopSpec.txt");
    ofstream s_cellPopSpec;
    s_cellPopSpec.open(respath + "/cellPopSpec.txt");
  

    // Is the number of generations for before fragmentation or after fragmentation?
    int numGen;
    if(fragmentation == 0) {numGen = numGenPre;} else {numGen = numGenPost;}

    // Start model dynamics
    for (int i = 0; i < numGen; i++) {
        // Each dynamic happens tmax times per generation per cell (tmax = 50)
        for (int t = 0; t < tmax; t++) {
            shuffle(cellOrder, numCells, eng);
            //Loop through each cell of the landscape
            for (int j = 0; j < numCells; j++) {
                int cell = cellOrder[j];
                kill(totalPopSpec, cellPopSpec, totalPop, cellPop,  cellRich, totalRich, probDeath, cell, numSpec, eng);
                reproduction(totalPopSpec, cellPopSpec, totalPop, cellPop, cellList, J, SvG, cell, numSpec, Rfr, eng);
                probDispDen = dispersalProb(cellPop, Rfr, cell, probDeath, probDisp);
                dispersal(totalPopSpec, cellPopSpec, totalPop, cellPop, cellRich, totalRich, distArray, disp, probDispDen, cell, numSpec, eng);
                if(fragmentation == 0) {
                    immigration(totalPopSpec, cellPopSpec, totalPop, cellPop, cellRich, totalRich, probImm, cell, numSpec, eng);
                } else {
                    immigration(totalPopSpec, cellPopSpec, totalPop, cellPop, cellRich, totalRich, probImmFrag, j, numSpec, eng);
                }
            }
        }

        // Live output to console
        if(i%25 == 0) { cout << "Gen: " << i << "/" << numGen << " | Total Pop: " << totalPop << " | Total Richness: " << totalRich << "\n";}
        
        if (i%25 == 0) {
            // After each generation store all outputs
            store2ColFiles(s_totalPop, i, totalPop);
            store2ColFiles(s_totalRich, i, totalRich);
            store3ColFiles(s_cellRich, i, numCells, cellRich);
            store3ColFiles(s_cellPop, i, numCells, cellPop);
            store3ColFiles(s_totalPopSpec, i, numSpec, totalPopSpec);
            if (i == 9975) {
                store4ColFiles(s_cellPopSpec, i, numCells, numSpec, cellPopSpec);
            }
        }
// comment out
        // Check if community is stable
 //       if(fragmentation == 0) {
 //           calculatePercPop(percPop, totalPopSpec, totalPop); // Calculate percentage of total pop each species makes up
 //           sortPop(percPop, numSpec);
 //           mainPop = findMainPops(percPop);
 //           if(prevPop == mainPop) {
 //               stableGens += 1;
 //           } else {
 //               stableGens = 0;
 //           }
 //           prevPop = mainPop;
 //       }

        // cout << stableGens << endl;

        // If the community has been stable for 1000 generations then stop running the model and save the final outputs
        // Stability is defined here as the same species making up 90% of the population for 1000 consecutive generations
 //       if(stableGens == 1000) break; 
//to here
    }

    // Store final outputs, for use in fragmentation runs
    if(fragmentation == 0) {

        // Remove populations of species which don't make up the "stable community" 
//comment out
//        keep_Stable_Species(totalPop, totalRich, totalPopSpec, cellPop, cellRich, cellPopSpec, mainPop, numCells, numSpec);
// to here
        storeNum(totalPop, "/final_totalPop.txt", finpath);
        storeNum(totalRich, "/final_totalRich.txt", finpath);
        storeArray<int>(cellPop, numCells, "/final_cellPop.txt", finpath);
        storeArray<int>(cellRich, numCells, "/final_cellRich.txt", finpath);
        storeArray<int>(totalPopSpec, numSpec, "/final_totalPopSpec.txt", finpath);
        store2DArray<int, numSpec>(cellPopSpec, numSpec, numCells, "/final_cellPopSpec.txt", finpath);
        
    }

    // Close all streams to files
    s_totalPop.close(); s_totalRich.close(); s_cellPop.close(); s_cellRich.close(); s_totalPopSpec.close(); s_cellPopSpec.close();

    // end timer FC
    cout << "Elapsed(s)=" << since(start).count() << endl; 
    return 0;
    

}

///////////////////////
//     Functions    //
//////////////////////

void fillCellList(int landscapeArray[][cellRows], int cellList[][2], int cellCols, int cellRows) {

    int i = 0;

    while (i < numCells) {
        for (int j = 0; j < cellRows; j++) {
            for (int k = 0; k < cellCols; k++) {
                cellList[i][1] = landscapeArray[j][k];
                i++;
            } 
        }
    }
}

void readNum(int &num, string fileName, string inpath) {

    ifstream file (inpath + fileName);

    file >> num;

}

void storeNum(int num, string fileName, string outpath) {

    ofstream file;
    file.open(outpath + fileName);

    file << num << "\n";

    file.close();

}

void store4ColFiles(ofstream &stream, int firstCol, int secondCol, int thirdCol, int fourthCol[][numSpec]) {

    for (int j = 0; j < secondCol; j++) {
        for (int k = 0; k < thirdCol; k++){
            stream << firstCol+1 << " " << j+1 << " " << k+1 << " " << fourthCol[j][k] << "\n";
        }
    }
}

void store3ColFiles(ofstream &stream, int firstCol, int secondCol, int thirdCol[]) {

    for (int j = 0; j < secondCol; j++) {
        stream << firstCol+1 << " " << j+1 << " " << thirdCol[j] << "\n";
    }
}

void store2ColFiles(ofstream &stream, int firstCol, int secondCol) {

    stream << firstCol+1 << " " << secondCol << "\n";

}

double dispersalProb(int cellPop[numCells],int Rfr, int cell, double probDeath, double probDisp) {

    double Nequ, pDispEff;
    pDispEff = probDisp;

    Nequ = Rfr*(3+log((1-probDeath)/probDeath));			//calculate density dependent Pmig if N/Nequ>.75

    if (cellPop[cell]/Nequ > 0.75) {
        pDispEff = 1/(1+(1-probDisp)*pow((1+probDisp),3)/pow(probDisp,4)*pow((probDisp/(1+probDisp)),(4*(double)cellPop[cell]/Nequ)));
    }

    return pDispEff;

}

vector <int> findValidCells(double (&distArray)[numCells][numCells], double distance, int cell) {

    vector<int>validCells;

    // Go through row of distArray for the cell and identify all cells within distance
    for (int i = 0; i < numCells; i++) {
        if(distArray[cell][i] <= distance) {
            validCells.push_back(i);
        }
    }
    return validCells;
}

void dispersal(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells],  int (&cellRich)[numCells], int &totalRich,
double (&distArray)[numCells][numCells], double (&disp)[numSpec], double prob, int cell, int numSpec, mt19937& eng) {

    if(cellPop[cell] > 0) { // Check cell isn't empty

        int chosenInd, dispCell;
        double dispAbility;
        vector<int> validCells;

        if (uniform(eng) <= prob) {
            chosenInd = randomInd(cellPopSpec, cellPop, numSpec, cell, eng);
            dispAbility = disp[chosenInd];

            if(dispAbility >= 1) {  // If dispAbility less than 1 then the individual can't disperse
                validCells = findValidCells(distArray, dispAbility, cell);
                int arr[validCells.size()]; 
                copy(validCells.begin(), validCells.end(), arr); // Copy into array for use in shuffle function
                shuffle(arr, validCells.size(), eng);                 // Shuffle valid cells into random order
                if(arr[0] != cell) {
                    dispCell = arr[0];                             // Choose first cell in shuffled order which isn't the original cell
                } else {
                    dispCell = arr[1];
                }

                // Move individual from cell to chosenCell
                if(cellPopSpec[cell][chosenInd] == 1) {cellRich[cell]--;}
                if(cellPopSpec[dispCell][chosenInd] == 0) {cellRich[dispCell]++;}
                cellPopSpec[cell][chosenInd]--;
                cellPopSpec[dispCell][chosenInd]++;
                cellPop[cell]--;
                cellPop[dispCell]++;                              
            }
        }

    } 
}

void getDistances(double (&distArray)[numCells][numCells], int landscapeCoords[][2], int cellRows, int cellCols) {

double x_diff, y_diff;

    for (int i = 0; i < numCells; i++) {
        for (int j = 0; j < numCells; j++) {
            x_diff = abs(landscapeCoords[i][0] - landscapeCoords[j][0]);
            if(x_diff > cellRows/2) {x_diff = cellRows - x_diff;}
            y_diff = abs(landscapeCoords[i][1] - landscapeCoords[j][1]);
            if(y_diff > cellCols/2) {y_diff = cellCols - y_diff;}
            distArray[i][j] = sqrt((x_diff*x_diff) + (y_diff*y_diff));
        }
    }
    
}

void cellCoords(int (&landscapeCoords)[numCells][2], int cols, int rows, int numCells) {

    int i = 0;

    while(i < numCells) {
        for (int j = 0; j < rows; j++) {
            for (int k = 0; k < cols; k++) {
                landscapeCoords[i][0] = j;
                landscapeCoords[i][1] = k;
                i++;
            }
        }
    }
}

double calculateInteractions(double (&J)[numSpec][numSpec], int (&cellPopSpec)[numCells][numSpec], int (&cellPop)[numCells], double (&distArray)[numCells][numCells],
    int cell, int numSpec, int ind) {

    double H;
    H = 0; 
    vector <int> validCells;
    
    // Need to add nearby cells to this as well, which will just be a set distance which will be in a matrix which will need to be a part of this function
    // and the reproduction function as a whole
    // Think I can add by simply making an array of nearby cells (always 4 neighbours, + itself, so 5)
    // then for loop through those, and then put the current for loop in that

    validCells = findValidCells(distArray, 0, cell);

    for (int j = 0; j < validCells.size(); j++) {
        for (int i = 0; i < numSpec; i++) {H += J[ind][i]*cellPopSpec[validCells[j]][i];}
    }
    
    // Currently dividing H by the number of cells interactions are taken from, which creates an average H from all cells with the chosenInd
    // Not sure if this is the right way to do it though. Could include the population of all chosen cells in the next calculation, but should the density of a neighbouring cell
    // influence the likelihood of reproduction of an individual?

    H = H/validCells.size();

    // Original code
    // for (int i = 0; i < numSpec; i++) {H += J[ind][i]*cellPopSpec[cell][i];}

    return H;

}

double specialism(int (&cellList)[numCells][2], double (&SvG)[numSpec], int cell, int ind) {
    
    double Mu;

    if(cellList[cell][1] == 1) {
        Mu = SvG[ind];
    } else {
        Mu = SvG[ind]*-1;
    }
    return Mu;
}

void reproduction(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells], 
int (&cellList)[numCells][2], double (&J)[numSpec][numSpec], double (&SvG)[numSpec] , int cell, int numSpec, int Rfr, mt19937& eng) {

    if(cellPop[cell] > 0) {

        int chosenInd;
        double H, pOff, Mu;

        chosenInd = randomInd(cellPopSpec, cellPop, numSpec, cell, eng);
        Mu = specialism(cellList, SvG, cell, chosenInd);
        H = calculateInteractions(J, cellPopSpec, cellPop, distArray, cell, numSpec, chosenInd);
        H = (H*weightInt/cellPop[cell]) - (cellPop[cell]/Rfr) + weightSpec*Mu; // 10 chosen as arbitrary carrying capactiy
        pOff = exp(H) / (1 + exp(H));

        if (uniform(eng) <= pOff) {
            totalPopSpec[chosenInd]++;
            cellPopSpec[cell][chosenInd]++;
            totalPop++;
            cellPop[cell]++;
        }
    }
}

void kill(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells],  int (&cellRich)[numCells], int &totalRich,
double prob, int cell, int numSpec, mt19937& eng) {
    
    if(cellPop[cell] > 0){ 

    int chosenInd;

        if (uniform(eng) <= prob) {
            chosenInd = randomInd(cellPopSpec, cellPop, numSpec, cell, eng);
            if (totalPopSpec[chosenInd] == 1) {totalRich--;}
            if(cellPopSpec[cell][chosenInd] == 1) {cellRich[cell]--;}
            if(totalPopSpec[chosenInd] > 0){totalPopSpec[chosenInd]--;}
            if(cellPopSpec[cell][chosenInd] > 0) {cellPopSpec[cell][chosenInd]--;}
            if(totalPop > 0) {totalPop--;}
            if(cellPop[cell] > 0) {cellPop[cell]--;}
        }
    }
}

// Choose a random individual (proportional to how many of that species there are)
int randomInd(int (&cellPopSpec)[numCells][numSpec], int (&cellPop)[numCells], int numSpec, int cell, mt19937& eng) {
    
    double sum, threshold;
    sum = 0;
    // Create threshold value by multiplying the total number of individuals in the cell by a number between 0-1
    threshold = uniform(eng)*cellPop[cell];

    // Get an individual of a random species (proportional to how many of that species there are) by summing the number of individuals of each species in the cell
    // and then choosing the species whose population get the sum to the threshold value
    for (int i = 0; i < numSpec; i++) {
        sum += cellPopSpec[cell][i];
        if (sum > threshold) {return i;}
    }
    cout << "Threshold for randomInd not hit, this is likely causing huge errors in results" << "\n";
    return 0;
}

void shuffle(int arr[], int arrElements, mt19937& eng) {
    
    int randomNum;

    for (int i = 0; i < arrElements; i++){
        randomNum = chooseInRange(0, arrElements-1, eng);
        swap(arr[i], arr[randomNum]);
    }
}

void immigration(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells],  int (&cellRich)[numCells], int &totalRich ,
double prob, int cell, int numSpec, mt19937& eng) {
    
    int chosenSpec;

    if(uniform(eng) <= prob) {  
        chosenSpec = chooseInRange(0, numSpec-1, eng);
        if(totalPopSpec[chosenSpec] == 0) {totalRich++;}
        if(cellPopSpec[cell][chosenSpec] == 0) {cellRich[cell]++;}
        totalPopSpec[chosenSpec]++;
        cellPopSpec[cell][chosenSpec]++;
        totalPop++;
        cellPop[cell]++;
    }
}

int chooseInRange(int a, int b, mt19937& eng) {
    uniform_int_distribution<> choose(a, b); // define the range [a,b], extremes included.
    return choose(eng);
}

void initialisePop(int (&totalPopSpec)[numSpec], int (&cellPopSpec)[numCells][numSpec], int &totalPop, int (&cellPop)[numCells],  int (&cellRich)[numCells], 
    int &totalRich, int numSpec, int numCells, mt19937& eng) {

    int chosenSpec;

    for (int i = 0; i < numCells; i++){
        for (int j = 0; j < initPop; j++) {
            chosenSpec = chooseInRange(0, numSpec-1, eng);
            if(totalPopSpec[chosenSpec] == 0) {totalRich++;}
            if(cellPopSpec[i][chosenSpec] == 0) {cellRich[i]++;}
            totalPopSpec[chosenSpec]++;
            cellPopSpec[i][chosenSpec]++;
            totalPop++;
            cellPop[i]++;
        }
    }
}

double uniform(mt19937& eng) {
    uniform_real_distribution<> rand(0, 1); // define the range [a,b], extremes included.
    return rand(eng);
}

double gaussian(mt19937& eng) {
    double u1, u2, normalNum;

    u1 = uniform(eng); // Create two random numbers to normally distribute
    u2 = uniform(eng);

    normalNum = 0.25*(sqrt(-2.0 * log(u1)) * cos(two_pi * u2)); // Create normally distributed vales with mean 0 and sd 0.25
    return normalNum;
}

void createJMatrix(double (&J)[numSpec][numSpec], double probInt, mt19937& eng) {
    for (int i = 0; i < numSpec; i++){
        for (int j = 0; j < numSpec; j++) {
            if(i == j || uniform(eng) <= probInt) {
                J[i][j] = 0;
            } else {
                J[i][j] = gaussian(eng);
            } 
        }
    } 
}

void createSvG(double (&SvG)[numSpec], mt19937& eng) {
    for (int i = 0; i < numSpec; i++) {
        SvG[i] = uniform(eng);
    }
}

void createDisp(double (&disp)[numSpec]) {
    for (int i = 0; i < numSpec; i++) {
        disp[i] = 0;
    }
}

void calculatePercPop(double (&percPop)[numSpec][2], int (&totalPopSpec)[numSpec], int totalPop){
    for (int i = 0; i < numSpec; i++) {
        percPop[i][0] = i;
        percPop[i][1] = (double)totalPopSpec[i]/(double)totalPop;
    }
}

void sortPop(double (&percPop)[numSpec][2], int numSpec) {
    
    int maxInd = 0;

    for (int i = 0; i < numSpec; i++) {
        maxInd = findMax(percPop, i, numSpec);
        swap(percPop[i][1], percPop[maxInd][1]);
        swap(percPop[i][0], percPop[maxInd][0]);
    }
    
}

double findMax(double percPop[numSpec][2], int low, int high) {

    int maxInd;
    double max;

    max = percPop[low][1];
    maxInd = low;

    for (int i = low; i < high; i++)
    {
        if(percPop[i][1] > max) {
            max = percPop[i][1];
            maxInd = i;
        }
    }
    return maxInd;
}

vector <int> findMainPops(double (&percPop)[numSpec][2]) {

    int i = 0;
    double percentage = 0;
   
    vector<int>mainPops;

    // Find species that make up 90%+ of the overall population
    while(percentage < 0.9) {
        percentage += percPop[i][1];
        mainPops.push_back(percPop[i][0]);
        i++;
    }

    // Then sort this vector so it can be check for equality
    sort(mainPops.begin(), mainPops.end());

    return mainPops;

    

}

void keep_Stable_Species(int &totalPop, int &totalRich, int (&totalPopSpec)[numSpec], int (&cellPop)[numCells], int (&cellRich)[numCells], 
    int (&cellPopSpec)[numCells][numSpec], vector <int> mainPop, int numCells, int numSpec) {

        bool stableSpec = 0;
        totalPop = 0;
        for (int i = 0; i < numSpec; i++){totalPopSpec[i] = 0;}
        

        for (int i = 0; i < numCells; i++){
            cellRich[i] = 0;
            cellPop[i] = 0;
            for (int j = 0; j < numSpec; j++){
                stableSpec = 0;
                for (int k = 0; k < mainPop.size(); k++) {
                    if(j == mainPop[k]) {
                        stableSpec = 1;
                    }
                }
                if(stableSpec == 1) {
                    totalPop += cellPopSpec[i][j];
                    totalPopSpec[j] += cellPopSpec[i][j];
                    cellPop[i] += cellPopSpec[i][j];
                    if(cellPopSpec[i][j] > 0) {cellRich[i] += 1;}
                } else {
                    cellPopSpec[i][j] = 0;
                }
            } 
        }
    
    totalRich = mainPop.size();
        

}