function [num_pts, final_pts] = do_folds(problem)
  num_folds = numel(problem.axes);
  num_pts = zeros(num_folds, 1);

  pts = problem.points;

  for fold_idx = 1:num_folds
    axis = problem.axes(fold_idx);
    pos = problem.positions(fold_idx);
    pts = fold(axis, pos, pts);
    pts = unique(pts, "rows");
    num_pts(fold_idx) = size(pts, 1);
  endfor

  final_pts = pts;
end

## Local Variables:
## mode: octave
## End:
