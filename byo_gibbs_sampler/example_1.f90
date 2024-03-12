program sampler
    ! my attempt at a translation of the build your own gibbs sampler notebook from AST5240 into fortran
    ! this is just exercise 1 (a simple 1D Gaussian)

    implicit none

    real, parameter :: pi = 4. * atan(1.)

    real, dimension(1000) :: dat
    real :: mu_true = 10.
    real :: sigma = 1.

    real, dimension(1000) :: mu_grid
    real, dimension(1000) :: lnL

    real :: u1, u2
    integer :: i ! iterator

    ! generate data from normal distribution using box-muller transformation
    normal_data: do i = 1, 1000
        call random_number(u1) ! generates random number from uniform distribution 0 < u <= 1
        call random_number(u2)
        u1 = 1. - u1 ! convert to uniform distribution 0 <= u < 1
        u2 = 1. - u2
        dat(i) = mu_true + sigma*sqrt(-2.*log(u1))*cos(2.*pi*u2)
    end do normal_data
    
    call lnL_Gaussian_1D(dat, sigma, mu_grid, lnL) 
    
    ! write results to a file so I can plot it with matplotlib
    open(10, file = "ex1_results.txt", status = "replace", action = "write")
        do i = 1, 1000
            write(10,100) dat(i),mu_grid(i),lnL(i)
            100 format (F10.6, " ", F10.6, " ", F13.6)
        end do
    close(10)

    print *,sum(dat)/1000. ! maximum likelihood solution is just the sample mean in this case

end program sampler

subroutine lnL_Gaussian_1D(dat, sigma, mu_grid, lnL)
    implicit none
    ! returns a one-dimensional Gaussian log-likelihood function
    ! for the mean of a given dataset

    real, parameter :: pi = 4. * atan(1.)

    real, dimension(1000), intent(in) :: dat ! dataset (I know assumed shape arrays are a thing but idk how to use them so explicit shape it is)
    real, intent(in) :: sigma ! standard deviation of dataset
    real, dimension(1000), intent(out) :: mu_grid ! grid of potential mean values
    real, dimension(1000), intent(out) :: lnL ! final log-likelihood for every potential mu

    real :: dat_min, dat_max ! max and min values of dataset
    real :: diff ! difference between max and min
    integer :: j ! iterator
    
    real :: p ! probability for given mu in mu_grid
    integer :: i ! iterator

    ! find the minimum and maximum values of the dataset
    dat_min = minval(dat)
    dat_max = maxval(dat)

    ! fill output arrays
    diff = dat_max - dat_min
    mu_iter: do j = 1, 1000
        p = 0
        mu_grid(j) = dat_min + diff * (j-1)/999. ! recreates numpy linspace
        data_iter: do i = 1, 1000
            p = p + log(1./sqrt(2.*pi*sigma**2)) - (dat(i) - mu_grid(j))**2 / (2.*sigma**2)
        end do data_iter
        lnL(j) = p
    end do mu_iter

end subroutine lnL_Gaussian_1D