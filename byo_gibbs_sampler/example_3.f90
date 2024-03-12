module samplers
    implicit none

    real, parameter :: pi = 4. * atan(1.)

    contains
    real function random_normal(mu, sigma)
        implicit none

        real :: mu
        real :: sigma
        real :: u1, u2

        call random_number(u1)
        call random_number(u2)
        u1 = 1. - u1
        u2 = 1. - u2

        random_normal = mu + sigma*sqrt(-2.*log(u1))*cos(2.*pi*u2)
        return
        end function random_normal

    subroutine return_log_likelihood(nfreq, dat, noise, spectrum, amplitude, lnL)
        implicit none

        real, parameter :: pi = 4. * atan(1.)

        integer, intent(in) :: nfreq ! number of frequencies
        real, dimension(nfreq), intent(in) :: dat ! data array (1 point for each frequency)
        real, dimension(nfreq), intent(in) :: noise ! uncertainty on each data point
        real, dimension(nfreq), intent(in) :: spectrum ! ratio between each frequency and the reference
        real, intent(in) :: amplitude ! multiply by spectrum to generate a model

        real, dimension(nfreq) :: model

        real, intent(out) :: lnL

        model = spectrum*amplitude

        lnL = sum(log(1/sqrt(2.*pi*noise**2)) - (dat - model)**2/(2.*noise**2))

    end subroutine return_log_likelihood

    subroutine solve_for_ML(nfreq, dat, noise, spectrum, niter, amplitude)
        implicit none

        integer, intent(in) :: nfreq ! number of frequencies
        real, dimension(nfreq), intent(in) :: dat ! data array (1 point for each frequency)
        real, dimension(nfreq), intent(in) :: noise ! uncertainty on each data point
        real, dimension(nfreq), intent(in) :: spectrum ! ratio between each frequency and the reference
        integer, intent(in) :: niter ! number of iterations
        real, intent(out) :: amplitude ! multiply by spectrum to generate a model

        real, dimension(nfreq, nfreq) :: ncov ! covariance matrix of noise
        real :: nmean  = sum(noise)/nfreq! mean of noise 
        integer :: i, j ! iterators

        real :: t1, t2 

        ! find the covariance matrix of the noise
        do i = 1, nfreq
            do j = 1, nfreq
                ncov(i, j) = sum((noise(i) - nmean) * (noise(j) - nmean))/(nfreq - 1)
            end do
        end do

        t1 = sum(matmul(matmul(transpose(spectrum), ncov), spectrum))
        t2 = sum(matmul(matmul(transpose(spectrum), ncov), dat))

        ! solve equation t1*a = t2 for a
        amplitude = t2/t1
        end subroutine solve_for_ML

    subroutine metropolis_sample(nfreq, dat, noise, spectrum, niter, amplitude_ini, samples)
        implicit none

        integer, intent(in) :: nfreq ! number of frequencies
        real, dimension(nfreq), intent(in) :: dat ! data array (1 point for each frequency)
        real, dimension(nfreq), intent(in) :: noise ! uncertainty on each data point
        real, dimension(nfreq), intent(in) :: spectrum ! ratio between each frequency and the reference
        integer, intent(in) :: niter ! number of iterations
        real, intent(in) :: amplitude_ini ! initial value of the Metropolis chain

        real :: amp_sample, amp_new ! amplitudes to check in Metropolis chain
        real, dimension(niter) :: samples
        real :: step_size = 0.2

        integer :: i ! iterator

        amp_sample = amplitude_ini
        metropolis_iter: do i = 1, niter
            amp_new = amp_sample + random_normal(0.0, step_size)

            ! finish metropolis algorithm
        end do metropolis_iter

    end subroutine solve_for_ML
end module samplers