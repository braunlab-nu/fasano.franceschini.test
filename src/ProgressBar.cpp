#include "ProgressBar.h"

/****************************************************************************/

ProgressBar::ProgressBar(unsigned int nsteps, bool verbose) :
    nsteps(nsteps), ticksPerStep(MAX_TICKS / nsteps + 1), verbose(verbose),
    ticks(0), progress(0) {
    if (!verbose) {
        return;
    }
    Rcpp::Rcerr << "0%   10   20   30   40   50   60   70   80   90   100%\n"
                << "|----|----|----|----|----|----|----|----|----|----|\n";
}

/****************************************************************************/

void ProgressBar::step() {
    if (!verbose) {
        return;
    }
    progress += 1 / static_cast<double>(nsteps);
    double lbound = ticks / static_cast<double>(MAX_TICKS);
    if (progress > lbound && ticks < MAX_TICKS) {
        for (unsigned int i = 0; i < ticksPerStep; ++i) {
            Rcpp::Rcerr << "*";
            if (++ticks == MAX_TICKS) {
                break;
            }
        }
    }
}

/****************************************************************************/

void ProgressBar::finalize() {
    if (!verbose) {
        return;
    }
    for (unsigned int i = ticks; i < MAX_TICKS; ++i) {
        Rcpp::Rcerr << "*";
    }
}

/****************************************************************************/
