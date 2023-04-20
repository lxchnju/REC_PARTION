program rec_partion
use rec_type
implicit none

    integer, parameter  :: p=6000, dim_back=30, m=8, n=8, rc_num_max=42
    type(Rectangle)  :: rect, rect_1, rect_2
    type(Rectangle), allocatable  :: rects(:)
    type(Point)  :: points(p)
    real :: a(n-1), b(m-1), len, hgt, temp, temp0
    real, allocatable :: x0(:), y0(:), xtemp(:), ytemp(:), x_final(:), y_final(:)
    integer :: p_o, i, j, cnt, it_num, rc, rc_temp, rc_init, rc_num, direct, maxp, minp!, max_c, min_c

    rect%lowerLeft%x=0.0; rect%lowerLeft%y=0.0
    rect%upperRight%x=1.0; rect%upperRight%y=1.0
    len = rect%upperRight%x-rect%lowerLeft%x
    hgt = rect%upperRight%y-rect%lowerLeft%y

    rect_1%lowerLeft%x=0.1;  rect_1%lowerLeft%y=0.1
    rect_1%upperRight%x=0.6; rect_1%upperRight%y=0.6

    rect_2%lowerLeft%x=0.4;  rect_2%lowerLeft%y=0.4
    rect_2%upperRight%x=0.9; rect_2%upperRight%y=0.9

    p_o=p-dim_back*dim_back

    call generateRandomPoints(p_o/4, rect_1, points(1:p_o/4))
    call generateRandomPoints(p_o*3/4, rect_2, points(p_o/4+1:p_o))
    do i = 1, dim_back
    do j = 1, dim_back
    points(p_o+1+(i-1)* dim_back+j-1)%x=(j-1)*len/(dim_back-1)+rect%lowerLeft%x
    points(p_o+1+(i-1)* dim_back+j-1)%y=(i-1)*len/(dim_back-1)+rect%lowerLeft%y
    enddo
    enddo
    it_num = 0
    allocate(x0(0))
    allocate(y0(0))
    allocate(rects(0))

    ytemp = [rect%lowerLeft%y, rect%upperRight%y]
DO WHILE(it_num <= log(m*1.0)/log(2.) )
    it_num = it_num + 1
    do i = 1, size(rects)
    x0 = [x0, best_single_x0(rects(i), points)]
    enddo
    xtemp = [rect%lowerLeft%x, x0, rect%upperRight%x]
    rects = split_rectangles(xtemp, ytemp)
    print*, xtemp
ENDDO
    ! deallocate(rects)

rc = huge(0)
rc_temp = 0
rc_num = 0
DO WHILE(rc_num < rc_num_max)
    rc_num = rc_num + 1
    it_num = 0
    ytemp = [rect%lowerLeft%y, rect%upperRight%y]
    deallocate(y0)
    allocate(y0(0))
DO WHILE(it_num < log(n*1.0)/log(2.) )!对每一列的矩形，挑选出最好的y0
    it_num = it_num + 1
    rects = best_group_y0(xtemp, ytemp, y0, points, rect)

    ! print*, ytemp
ENDDO

    it_num = 0
    xtemp = [rect%lowerLeft%x, rect%upperRight%x]
    deallocate(x0)
    allocate(x0(0))
DO WHILE(it_num < log(m*1.0)/log(2.) )!对每一列的矩形，挑选出最好的y0
    it_num = it_num + 1
    rects = best_group_x0(xtemp, ytemp, x0, points, rect)
    
    ! print*, xtemp
ENDDO

    rc_temp = vari_conunts_in_rects(xtemp,ytemp,points,direct, maxp, minp)
    rc_init = vari_conunts_in_rects([(i*len/m,i=0,m)],[(i*hgt/n,i=0,n)],points,direct)
    if(rc > rc_temp .and. minp .gt. 0)then
    ! if(rc > rc_temp )then
     rc =  rc_temp
     x_final = xtemp
     y_final = ytemp
    print*,  rc_temp, rc_init, minp, '迭代第', rc_num, '次', rc_num_max, '次'
    endif
ENDDO

!     deallocate(rects)

!     it_num = 0
!     allocate(rects(0))
! DO WHILE(it_num <= log(n*1.0)/log(2.) )
!     it_num = it_num + 1
!     do i = 1, size(rects)
!     y0 = [y0, best_single_y0(rects(i), points)]
!     enddo
!     ! xtemp = [rect%lowerLeft%x, x0, rect%upperRight%x]
!     ytemp = [rect%lowerLeft%y, y0, rect%upperRight%y]
!     rects = split_rectangles([rect%lowerLeft%x, rect%upperRight%x], ytemp)
!     print*, ytemp
! ENDDO

    deallocate(rects)
    deallocate(x0)
    deallocate(y0)

    open(unit=1, file='rec_cords.txt')
    write(1,'(99F12.8)') x_final
    write(1,'(99F12.8)') y_final
    write(1,*)'------------------------------'
    do i=1, p
    write(1,*)points(i)%x,points(i)%y
    enddo
    close(1)

end program rec_partion