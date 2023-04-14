// Create a simple Rcpp progress bar

#ifndef PROGRESS_H
#define PROGRESS_H

#include <Rcpp.h>

class ProgressBar {
private:
    // Number times the progress bar will be incremented
    const unsigned int nsteps;
    // When printing, how many ticks should be printed
    const unsigned int ticksPerStep;
    // Whether to display output
    const bool verbose;
    // How many ticks have been printed so far
    unsigned int ticks;
    // Percentage of task completed so far
    double progress;
    // Number of ticks to print
    static const unsigned int MAX_TICKS = 51;
public:
    ProgressBar(unsigned int nsteps, bool verbose);
    // Increment the progress bar
    void step();
    // Double check all ticks have been printed at the end
    void finalize();
};

#endif //PROGRESS_H
