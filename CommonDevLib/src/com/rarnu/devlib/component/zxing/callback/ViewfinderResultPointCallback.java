package com.rarnu.devlib.component.zxing.callback;

import com.google.zxing.ResultPoint;
import com.google.zxing.ResultPointCallback;
import com.rarnu.devlib.component.zxing.view.ViewfinderView;

public final class ViewfinderResultPointCallback implements ResultPointCallback {

  private final ViewfinderView viewfinderView;

  public ViewfinderResultPointCallback(ViewfinderView viewfinderView) {
    this.viewfinderView = viewfinderView;
  }

  @Override
public void foundPossibleResultPoint(ResultPoint point) {
    viewfinderView.addPossibleResultPoint(point);
  }

}
