// Create a simple Rcpp progress bar

#ifndef PROGRESS_H
#define PROGRESS_H

#include <Rcpp.h>

class ProgressBar {
private:
    // Number times the progress bar will be incremented
    const int nsteps;
    // When printing, how many ticks should be printed
    const int ticksPerStep;
    // Whether to display output
    const bool verbose;
    // How many ticks have been printed so far
    int ticks;
    // Percentage of task completed so far
    double progress;
    // Number of ticks to print
    static const int MAX_TICKS = 51;
public:
    ProgressBar(int nsteps, bool verbose) :
        nsteps(nsteps), ticksPerStep(MAX_TICKS/nsteps+1), verbose(verbose), ticks(0), progress(0) {
        if (verbose) {
            Rcpp::Rcerr << "0%   10   20   30   40   50   60   70   80   90   100%\n" <<
                           "|----|----|----|----|----|----|----|----|----|----|\n";
        }
    }

    void step() {
        if (verbose) {
            progress += 1/static_cast<double>(nsteps);
            if (progress > ticks/static_cast<double>(MAX_TICKS) && ticks < MAX_TICKS) {
                for (int i = 0; i < ticksPerStep; ++i) {
                    Rcpp::Rcerr << "*";
                    ++ticks;
                    if (ticks == MAX_TICKS) {
                        break;
                    }
                }
            }
        }
    }

    // Double check all ticks have been printed at the end
    void finalize() {
        if (verbose) {
            for (int i = ticks; i < MAX_TICKS; ++i) {
                Rcpp::Rcerr << "*";
            }
        }
    }
};

#endif //PROGRESS_H
