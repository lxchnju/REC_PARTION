module rec_type
  implicit none
  Integer , parameter :: int64 = Selected_Int_Kind( 10 )
  Integer , parameter ::step_temp = 72

  ! Define a point in two-dimensional space
  type Point
    real :: x, y
  end type Point

  ! Define a rectangle in two-dimensional space
  type Rectangle
    type(Point) :: lowerLeft, upperRight
  end type Rectangle

contains

  subroutine generateRandomPoints(m, rect, points)
    integer, intent(in) :: m
    type(Rectangle), intent(in) :: rect
    type(Point), intent(out) :: points(m)
    
    integer :: i
    real :: x, y, rand_theta, rand_dia, dia, x0, y0

    dia = max((rect%upperRight%x - rect%lowerLeft%x), (rect%upperRight%y - rect%lowerLeft%y))
    x0 = (rect%upperRight%x + rect%lowerLeft%x)/2
    y0 = (rect%upperRight%y + rect%lowerLeft%y)/2
      call Init_Random_Seed()

    do i = 1, m
      ! Generate random coordinates within the rectangle
      call random_number(rand_theta); rand_theta = rand_theta*2*3.1415926536
      call random_number(rand_dia)
      x = rand_dia*dia/2*cos(rand_theta)+x0
      ! x = rect%lowerLeft%x + rand*(rect%upperRight%x - rect%lowerLeft%x)
      ! call random_number(rand); rand = rand*2*3.1415926536
      y = rand_dia*dia/2*sin(rand_theta)+y0
      ! y = rect%lowerLeft%y + rand*(rect%upperRight%y - rect%lowerLeft%y)
      ! print*, x-x0, y-y0, ((x-x0)**2+(y-y0)**2)/dia**2
      ! Set the coordinates of the point
      points(i)%x = x
      points(i)%y = y
    end do

  end subroutine generateRandomPoints

  function count_points_in_rect(rect, points) result(num_points)
  implicit none
  type(Rectangle), intent(in) :: rect
  type(point), dimension(:), intent(in) :: points
  integer :: num_points, i
  
  num_points = 0
  do i = 1, size(points)
    if (points(i)%x >= rect%lowerLeft%x  .and. &
        points(i)%x <= rect%upperRight%x .and. &
        points(i)%y >= rect%lowerLeft%y  .and. &
        points(i)%y <= rect%upperRight%y ) then
      num_points = num_points + 1
    end if
  end do
  
end function count_points_in_rect

function split_rectangles(a, b) result(rectangles)
  implicit none
  real, dimension(:) :: a, b
  type(rectangle), dimension((size(a)-1)*(size(b)-1)) :: rectangles
  integer :: i, j, k
  real :: temp

  ! 冒泡排序算法
  do i = 1, size(a)-1
    do j = 1, size(a)-i
      if (a(j) > a(j+1)) then
        temp = a(j)
        a(j) = a(j+1)
        a(j+1) = temp
      end if
    end do
  end do

  do i = 1, size(b)-1
    do j = 1, size(b)-i
      if (b(j) > b(j+1)) then
        temp = b(j)
        b(j) = b(j+1)
        b(j+1) = temp
      end if
    end do
  end do
  
  k = 0
  do i = 1, size(a)-1
    do j = 1, size(b)-1
      k = k + 1
      rectangles(k)%lowerLeft%x  = a(i)
      rectangles(k)%upperRight%x = a(i+1)
      rectangles(k)%lowerLeft%y  = b(j)
      rectangles(k)%upperRight%y = b(j+1)
    end do
  end do
  
end function split_rectangles

function vari_conunts_in_rects(x,y,points,direct, maxp, minp) result(vari)
    real :: x(:), y(:)
    integer :: i, cnt, vari,max_c, min_c, direct
    integer, optional :: maxp, minp
    type(Rectangle), allocatable  :: rects(:)
    type(Point)  :: points(:)
    max_c=0
    min_c=huge(0)
    rects = split_rectangles(x,y)
    do i = 1, size(rects)
    cnt = count_points_in_rect(rects(i), points)
    ! print*, rects(i)
    if(max_c < cnt) max_c = cnt
    if(min_c > cnt) min_c = cnt
    end do
    vari = max_c-min_c
    if(present(maxp))maxp = max_c
    if(present(minp))minp = min_c
    direct = count_points_in_rect(rects(size(rects)), points)-count_points_in_rect(rects(1), points)
end function vari_conunts_in_rects


function best_single_x0(rect, points)result(x0)
      real :: x0, y0, temp1, temp2
      real, allocatable :: x(:), y(:)
      integer :: direct, rc, rc_temp, it_num
      type(Rectangle)  :: rect
      type(Point)  :: points(:)

      it_num = 0
      rc = huge(0)
      rc_temp = 0

      DO WHILE(rc /= rc_temp)
      it_num = it_num + 1
      if(mod(it_num, 100) .EQ. 0) rc_temp = rc
      ! [rect%lowerLeft%x,a, rect%upperRight%x],[rect%lowerLeft%y,b, rect%upperRight%y]
      if(it_num == 1) then
        allocate(x(0))
        allocate(y(0))
        x = [rect%lowerLeft%x, rect%upperRight%x]
        y = [rect%lowerLeft%y, rect%upperRight%y]
        temp1 = x(1)
        temp2 = x(size(x))
        x0 = (temp1+temp2)/2.
      endif

      x = [x(1), x0, x(size(x))]
      rc = min(rc, vari_conunts_in_rects(x, y, points, direct))
      if(direct > 0) then
      temp1 = x0
      x0 = (temp1+temp2)/2.
      elseif(direct < 0) then
      temp2 = x0
      x0 = (temp1+temp2)/2.
      endif

      ! print*, x
      ! print*, rc, direct, it_num
      ENDDO
end function best_single_x0

function best_group_x0(xtemp, ytemp, x0, points, rect)result(rects)
    real, allocatable :: x0(:), y0(:), xtemp(:), ytemp(:)
    type(Rectangle), allocatable  :: rects(:)
    type(Rectangle)  :: rect
    type(Point)  :: points(:)
    real :: temp, temp0, temp_max, temp_min
    integer :: i, j, it_num, rc, rc_temp, direct, maxp, minp!, step_temp!, max_c, min_c

    rects = split_rectangles(xtemp, ytemp)
    do i = 1, size(xtemp)-1
    rc = huge(0)
    temp_max = 0.
    temp_min = huge(0.)
    do j = 1, size(ytemp)-1
    temp =  best_single_x0(rects((i-1)*(size(ytemp)-1)+j), points)
    rc_temp = vari_conunts_in_rects([xtemp(i), temp, xtemp(i+1)], ytemp, points,direct, maxp, minp)
    ! print*, rc, rc_temp
    if(rc > rc_temp .and. minp .gt. 0) then
     ! print*, rc, rc_temp
     rc = rc_temp
     temp0 = temp
    endif
    if(temp_min > temp) then
     temp_min = temp
    endif
    if(temp_max < temp) then
     temp_max = temp
    endif
    enddo

    do j = 2, step_temp-1
        temp =  temp_min+(temp_max-temp_min)/(step_temp-1)*(j-1)
        rc_temp = vari_conunts_in_rects([xtemp(i), temp, xtemp(i+1)], ytemp, points,direct, maxp, minp)
        ! if(minp .eq. 0) CYCLE
        if(rc > rc_temp .and. minp .gt. 0) then
         ! print*, rc, rc_temp
         rc = rc_temp
         temp0 = temp
        endif
    enddo

    x0 = [x0, temp0]
    enddo
    xtemp = [rect%lowerLeft%x, x0, rect%upperRight%x]
    rects = split_rectangles(xtemp, ytemp)
end function best_group_x0

function best_single_y0(rect, points)result(y0)
      real :: x0, y0, temp1, temp2
      real, allocatable :: x(:), y(:)
      integer :: direct, rc, rc_temp, it_num
      type(Rectangle)  :: rect
      type(Point)  :: points(:)

      it_num = 0
      rc = huge(0)
      rc_temp = 0

      DO WHILE(rc /= rc_temp)
      it_num = it_num + 1
      if(mod(it_num, 100) .EQ. 0) rc_temp = rc
      ! [rect%lowerLeft%x,a, rect%upperRight%x],[rect%lowerLeft%y,b, rect%upperRight%y]
      if(it_num == 1) then
        allocate(x(0))
        allocate(y(0))
        x = [rect%lowerLeft%x, rect%upperRight%x]
        y = [rect%lowerLeft%y, rect%upperRight%y]
        temp1 = y(1)
        temp2 = y(size(y))
        y0 = (temp1+temp2)/2.
      endif

      y = [y(1), y0, y(size(y))]
      rc = min(rc, vari_conunts_in_rects(x, y, points, direct))
      if(direct > 0) then
      temp1 = y0
      y0 = (temp1+temp2)/2.
      elseif(direct < 0) then
      temp2 = y0
      y0 = (temp1+temp2)/2.
      endif

      ! print*, x
      ! print*, rc, direct, it_num
      ENDDO
end function best_single_y0

function best_group_y0(xtemp, ytemp, y0, points, rect)result(rects)
    real, allocatable :: x0(:), y0(:), xtemp(:), ytemp(:)
    type(Rectangle), allocatable  :: rects(:)
    type(Rectangle)  :: rect
    type(Point)  :: points(:)
    real :: temp, temp0, temp_max, temp_min
    integer :: i, j, it_num, rc, rc_temp, direct, maxp, minp!, step_temp!, max_c, min_c

    rects = split_rectangles(xtemp, ytemp)
    do j = 1, size(ytemp)-1
    rc = huge(0)
    do i = 1, size(xtemp)-1
    temp =  best_single_y0(rects((i-1)*(size(ytemp)-1)+j), points)
    rc_temp = vari_conunts_in_rects(xtemp, [ytemp(j), temp, ytemp(j+1)], points,direct, maxp, minp)
    ! print*, rc, rc_temp
    if(rc > rc_temp .and. minp .gt. 0) then
     ! print*, rc, rc_temp
     rc = rc_temp
     temp0 = temp
    endif
    if(temp_min > temp) then
     temp_min = temp
    endif
    if(temp_max < temp) then
     temp_max = temp
    endif
    enddo

    do i = 2, step_temp-1
        temp =  temp_min+(temp_max-temp_min)/(step_temp-1)*(i-1)
        rc_temp = vari_conunts_in_rects(xtemp, [ytemp(j), temp, ytemp(j+1)], points,direct, maxp, minp)
        ! if(minp .eq. 0) CYCLE
        if(rc > rc_temp .and. minp .gt. 0) then
         ! print*, rc, rc_temp
         rc = rc_temp
         temp0 = temp
        endif
    enddo

    y0 = [y0, temp0]
    enddo
    ytemp = [rect%lowerLeft%y, y0, rect%upperRight%y]
    rects = split_rectangles(xtemp, ytemp)
end function best_group_y0

function generate_sorted_random_array(n) result(arr)
  implicit none
  integer, intent(in) :: n
  real, dimension(n) :: arr
  integer :: i, j
  real :: temp

  call Init_Random_Seed()
  ! do i = 1, n
  !   arr(i) = random_number()
  call random_number(arr)
  ! end do
  ! 冒泡排序算法
  do i = 1, n-1
    do j = 1, n-i
      if (arr(j) > arr(j+1)) then
        temp = arr(j)
        arr(j) = arr(j+1)
        arr(j+1) = temp
      end if
    end do
  end do
end function generate_sorted_random_array

Subroutine Init_Random_Seed()   
    integer :: ised , i , pid
    integer(int64) :: t
    real :: time
    integer , allocatable :: sed(:)
    ! call random_seed( size = ised ) !// 获得种子大小
    ised = 999
    allocate( sed(ised) ) !// 分配种子
    call system_clock(t) !// 获得时间
    ! call cpu_time(time)
    ! t = nint(time*999)
    pid = 13298543!getpid() !// 获得处理器ID
  t = ieor(t, int(pid, kind(t))) !// 用 pid 和日期做XOR运算

  do i = 1, ised
      sed(i) = lcg(t) !// 用线性同余计算种子
  end do
  call random_seed( put=sed ) !// 给定种子 

End Subroutine Init_Random_Seed 

 

Function lcg(s) !// 线性同余算法 Linear congruential generator
  integer :: lcg
  integer(int64) :: s

  if (s == 0) then
     s = 104729
  else
     s = mod(s, 4294967296_int64)
  end if

  s = mod(s * 279470273_int64, 4294967291_int64)
  lcg = int(mod(s, int(huge(0), int64)), kind(0))
End Function lcg

end module rec_type
