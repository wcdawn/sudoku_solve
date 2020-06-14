close all;
clear;

% solve a sudoku puzzle using "backfill" technique

% read a sudoku file and store it in input_read
% should be space delimited with blank squares represented by zeros
fname = input("Enter a sudoku file name:", "s");

input_arr = dlmread(fname);

% create a logical array indicating whether or not values are original
original = true(9,9);
original(input_arr == 0) = false;

% make a copy to not overwrite input_arr
work = input_arr;

% variables to keep track of the small 3x3 square where we're working
bound_l = 0; % boundary left
bound_r = 0; % boundary right
bound_u = 0; % boundary up
bound_d = 0; % boundary down

% initialize position of the cell we're examining
x = 1;
y = 1;

% begin by moving "forward" (left-to-right and up-to-down)
direction = 1;

tic;

max_iteration = 1000; % limit to 1000 iterations should be plenty
solved = false;
for i = 1:max_iteration

  if (~original(y,x))
    switch (y)
      case num2cell(1:3)
        bound_l = 1;
        bound_r = 3;
      case num2cell(4:6)
        bound_l = 4;
        bound_r = 6;
      case num2cell(7:9)
        bound_l = 7;
        bound_r = 9;
    end % switch y
    switch (x)
      case num2cell(1:3)
        bound_u = 1;
        bound_d = 3;
      case num2cell(4:6)
        bound_u = 4;
        bound_d = 6;
      case num2cell(7:9)
        bound_u = 7;
        bound_d = 9;
    end % switch x

    if (work(y,x) == 9)
      % cannot increment past 9
      work(y,x) = 0;
      x = x + direction;
      if (x == 0)
        % need to move up one row
        x = 9;
        y = y - 1;
      elseif (x == 10)
        % need to move down one row
        x = 1;
        y = y + 1;
      end
      continue
    end % work(y,x) == 9

    for guess = (work(y,x)+1):9
      work(y,x) = guess;
      bad = false;

      % row check
      for a = 1:9
        if ((work(y,x) == work(y,a)) && (a ~= x))
          bad = true;
          break
        end
      end

      % column check
      for a = 1:9
        if ((work(y,x) == work(a,x)) && (a ~= y))
          bad = true;
          break
        end
      end

      % "sector" (square) check
      for a = bound_l:bound_r
        for b = bound_u:bound_d
          if ((work(y,x) == work(a,b)) && (a ~= y) && (b ~= x))
            bad = true;
            break
          end
        end
      end

      if (~bad)
        % good guess
        direction = 1;
        break
      elseif ((bad) && (guess == 9))
        % couldn't guess
        work(y,x) = 0;
        direction = -1;
      end
    end % for guess
  end % if ~original

  x = x + direction;
  if (x == 10)
    x = 1;
    y = y + 1;
  elseif (x == 0)
    x = 9;
    y = y - 1;
  end

  if (y == 10)
    solved = true;
    break
  end

end

toc;

if (~solved) 
  error("failed to solve in maximum allowed iterations");
end

fprintf("completed in %d iterations\n", i);

fprintf("solution\n");
disp(work);
