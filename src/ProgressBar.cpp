#include "ProgressBar.h"

ProgressBar::ProgressBar(int nsteps, bool verbose) :
    nsteps(nsteps), ticksPerStep(MAX_TICKS/nsteps+1), verbose(verbose), ticks(0), progress(0) {
    if (verbose) {
        Rcpp::Rcerr << "0%   10   20   30   40   50   60   70   80   90   100%\n" <<
            "|----|----|----|----|----|----|----|----|----|----|\n";
    }
}

void ProgressBar::step() {
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

void ProgressBar::finalize() {
    if (verbose) {
        for (int i = ticks; i < MAX_TICKS; ++i) {
            Rcpp::Rcerr << "*";
        }
    }
}
