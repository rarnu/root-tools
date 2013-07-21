package com.sbbs.me.android.component;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.egit.github.core.TreeEntry;

import android.content.Context;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.HorizontalScrollView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.R;

public class PathView extends HorizontalScrollView implements OnClickListener {

	List<TreeEntry> listPath;
	RelativeLayout layBase;

	public PathView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	public PathView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public PathView(Context context) {
		super(context);
		init();
	}

	private void init() {
		setHorizontalScrollBarEnabled(false);
		listPath = new ArrayList<TreeEntry>();
		int padding = UIUtils.dipToPx(4);
		setPadding(padding, padding, padding, padding);
		layBase = new RelativeLayout(getContext());
		layBase.setLayoutParams(new ViewGroup.LayoutParams(
				FrameLayout.LayoutParams.MATCH_PARENT,
				FrameLayout.LayoutParams.MATCH_PARENT));
		addView(layBase);
	}
	
	public void postInvalidate() {
		this.buildUI();
	}

	private void buildUI() {
		layBase.removeAllViews();
		int viewId = 300000;
		for (int i = 0; i < listPath.size(); i++) {
			TextView tv = new TextView(getContext());
			tv.setId(viewId);
			RelativeLayout.LayoutParams fllp = new RelativeLayout.LayoutParams(
					LinearLayout.LayoutParams.WRAP_CONTENT,
					LinearLayout.LayoutParams.MATCH_PARENT);

			if (viewId > 300000) {
				fllp.addRule(RelativeLayout.RIGHT_OF, viewId - 1);
			}
			fllp.rightMargin = UIUtils.dipToPx(2);
			tv.setLayoutParams(fllp);
			tv.setGravity(Gravity.CENTER_VERTICAL);
			tv.setClickable(true);
			tv.setBackgroundResource(R.drawable.action_button_style);
			tv.setTextColor(getResources().getColor(R.color.google_dark_green));
			tv.setTextSize(16);
			if (i + 1 < listPath.size()) {
				tv.setText(listPath.get(i).getPath() + "/");
			} else {
				tv.setText(listPath.get(i).getPath());
			}
			tv.setTag(listPath.get(i));
			tv.setOnClickListener(this);
			layBase.addView(tv);
			viewId++;
		}
	}

	public void addPath(TreeEntry path) {
		listPath.add(path);
		this.postInvalidate();
	}

	public void upLevel() {
		listPath.remove(listPath.size() - 1);
		this.postInvalidate();
	}

	public void gotoPath(TreeEntry path) {
		List<TreeEntry> tmp = new ArrayList<TreeEntry>();
		for (int i = 0; i < listPath.size(); i++) {
			tmp.add(listPath.get(i));
			if (listPath.get(i).getSha().equals(path.getSha())) {
				break;
			}
		}
		listPath.clear();
		listPath.addAll(tmp);
		this.postInvalidate();
	}

	public interface PathClickListener {
		void onPathClick(TreeEntry entry);
	}

	private PathClickListener pathListener;

	public void setPathClickListener(PathClickListener listener) {
		this.pathListener = listener;
	}

	@Override
	public void onClick(View v) {
		if (pathListener != null) {
			pathListener.onPathClick((TreeEntry) v.getTag());
		}
	}

}
