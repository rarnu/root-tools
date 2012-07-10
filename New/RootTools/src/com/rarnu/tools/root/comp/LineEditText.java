package com.rarnu.tools.root.comp;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.widget.EditText;

public class LineEditText extends EditText {

	// [region] field define
	private Paint mPaint;
	// [/region]

	// [region] constructor
	public LineEditText(Context context, AttributeSet attrs) {
		super(context, attrs);
		mPaint = new Paint();

		mPaint.setStyle(Paint.Style.STROKE);
		mPaint.setColor(Color.BLACK);
	}

	// [/region]
	
	// [region] common
	@Override
	public void onDraw(Canvas canvas) {
		super.onDraw(canvas);
		canvas.drawLine(0, this.getHeight() - 1, this.getWidth() - 1,
				this.getHeight() - 1, mPaint);
	}
	// [/region]
}