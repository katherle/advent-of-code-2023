program sampler
    ! generates a multifrequency dataset and finds the best fit line passing through all data points
    implicit none

    real, parameter :: pi = 4. * atan(1.)

    integer :: i
    integer :: j ! iterators
    integer :: k
    integer :: nfreq = 5 ! number of frequencies
    integer :: ndraws = 1000
    real, dimension(5) :: nus = [(k*10, k=1,5)] ! 10 through 50 in intervals of 10 using implied do loop
    real :: nuref = 20. ! reference frequency

    real :: mu_true = 10.
    real, dimension(5) :: sigmas = [1., 1.5, 1.25, 2., 0.75]
    real, dimension(5, 1000) :: dat
    real, dimension(5) :: data_mean
    real, dimension(5) :: data_error

    real, dimension(1000) :: mu_grid
    real, dimension(1000) :: lnL

    real :: u1, u2 ! for box-muller transformation

    ! data is normally distributed at each frequency
    freq_iter: do i = 1, nfreq
        data_gen: do j = 1, ndraws
            call random_number(u1)
            call random_number(u2)
            u1 = 1. - u1
            u2 = 1. - u2
            dat(i, j) = mu_true*nus(i)/nuref + sigmas(i)*sqrt(-2.*log(u1))*cos(2.*pi*u2)
        end do data_gen
        data_mean(i) = sum(dat(i, :))/ndraws
        data_error(i) = sqrt(sum(abs(dat(i, :) - data_mean(i))**2)/ndraws)
    end do freq_iter
    print *,data_mean
    print *,data_error

    call multifrequency_gaussian(nfreq, nus, nuref, data_mean, data_error, mu_grid, lnL)
    
    open(10, file = "ex2_results.txt", status = "replace", action = "write")
        do i = 1, 1000
            write(10, 100) mu_grid(i),lnL(i)
            100 format(F10.6, " ", F13.6)
        end do
    close(10)

    print *,"Maximum likelihood solution: ",sum(data_mean)/sum(nus/nuref)

end program sampler

subroutine multifrequency_gaussian(nfreq, nu, nuref, dat, sigma, mu_grid, lnL)
    ! returns the Gaussian log-likelihood for the best fit line passing through a multifrequency dataset

    implicit none

    real, parameter :: pi = 4. * atan(1.)

    integer, intent(in) :: nfreq ! number of frequencies
    real, dimension(nfreq), intent(in) :: dat ! data array (1 point for each frequency)
    real, dimension(nfreq), intent(in) :: sigma ! standard deviation of data at each frequency

    real, dimension(nfreq), intent(in) :: nu ! frequency array
    real, intent(in) :: nuref ! reference frequency

    real, dimension(1000), intent(out) :: mu_grid ! grid of potential mean values
    real, dimension(1000), intent(out) :: lnL ! log-likelihood function for each mu and all frequencies

    real :: dat_min ! minimum value of dataset
    real :: dat_max ! maximum value of dataset
    real :: diff

    integer :: j, i ! iterators
    real :: p ! probability for given mu in mu_grid for each frequency
    real :: aobs ! observable mean

    dat_min = minval(dat)
    dat_max = maxval(dat)
    diff = dat_max - dat_min

    mu_iter: do j = 1, 1000
        p = 0.
        mu_grid(j) = dat_min + diff * (j-1)/999. ! recreates numpy linspace
        dat_iter: do i = 1, nfreq
            aobs = mu_grid(j)*nu(i)/nuref
            p = p + log(1./(2.*pi*sigma(i))) - (dat(i) - aobs)**2/(2.*sigma(i)**2) ! gaussian likelihood
        end do dat_iter
        lnL(j) = p
    end do mu_iter

end subroutine multifrequency_gaussian