package com.rarnu.zoe.loving.base;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.widget.RelativeLayout;

public abstract class BasePage extends RelativeLayout {

	protected int rootLayout = 0;
	protected LayoutInflater rootInflater;

	public BasePage(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		initSelf();
	}

	public BasePage(Context context, AttributeSet attrs) {
		super(context, attrs);
		initSelf();
	}

	public BasePage(Context context) {
		super(context);
		initSelf();
	}

	protected void initSelf() {
		requireRootLayoutId();
		RelativeLayout.LayoutParams laySelf = new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
		setLayoutParams(laySelf);

		RelativeLayout layRoot = (RelativeLayout) inflate(getContext(),
				rootLayout, null);
		RelativeLayout.LayoutParams rootParam = new RelativeLayout.LayoutParams(
				LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);
		layRoot.setLayoutParams(rootParam);
		addView(layRoot);
		
//		TextView tvLine = new TextView(getContext());
//		RelativeLayout.LayoutParams lineParam = new RelativeLayout.LayoutParams(1, LayoutParams.MATCH_PARENT);
//		lineParam.addRule(RelativeLayout.ALIGN_PARENT_RIGHT, 1);
//		tvLine.setLayoutParams(lineParam);
//		tvLine.setBackgroundColor(Color.GRAY);
//		addView(tvLine);

		init();
	}

	protected abstract void requireRootLayoutId();

	protected abstract void init();
	
	public abstract void load(String ... param);
	
	public abstract void refresh();
	
	public abstract void delete(int index);

	public void setRootInflater(LayoutInflater inflater) {
		this.rootInflater = inflater;
	}

	public LayoutInflater getRootInflater() {
		return this.rootInflater;
	}

}
