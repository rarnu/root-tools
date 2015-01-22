package com.zoe.calendar.fragment;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.RelativeLayout;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.R;
import com.zoe.calendar.utils.ResourceUtils;

public class NewbieFragment extends BaseFragment implements OnClickListener {

	Button btnStart;
	ImageView imgTop, imgBottom, imgTop2;

	public NewbieFragment() {
		super();
		// tagText = ResourceUtils.getString(R.tag.fragment_newbie);
	}
	public NewbieFragment(String tag) {
		super(tag, "");
	}

	@Override
	public int getBarTitle() {
		return 0;
	}

	@Override
	public int getBarTitleWithPath() {
		return 0;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		imgTop = (ImageView) innerView.findViewById(R.id.imgTop);
		imgBottom = (ImageView) innerView.findViewById(R.id.imgBottom);
		btnStart = (Button) innerView.findViewById(R.id.btnStart);
		imgTop2 = (ImageView) innerView.findViewById(R.id.imgTop2);

		RelativeLayout.LayoutParams rlpTop = (RelativeLayout.LayoutParams) imgTop
				.getLayoutParams();
		rlpTop.height = UIUtils.getWidth() * 4 / 5;
		imgTop.setLayoutParams(rlpTop);

		RelativeLayout.LayoutParams rlpBottom = (RelativeLayout.LayoutParams) imgBottom
				.getLayoutParams();
		rlpBottom.height = UIUtils.getWidth() * 5 / 36;
		imgBottom.setLayoutParams(rlpBottom);
		
		RelativeLayout.LayoutParams rlpTop2 = (RelativeLayout.LayoutParams) imgTop2.getLayoutParams();
		rlpTop2.height = UIUtils.getWidth();
		imgTop2.setLayoutParams(rlpTop2);

		RelativeLayout.LayoutParams rlpStart = (RelativeLayout.LayoutParams) btnStart
				.getLayoutParams();
		rlpStart.height = UIUtils.getWidth() * 8 / 45;
		btnStart.setLayoutParams(rlpStart);
		
		
	}

	@Override
	public void initEvents() {
		btnStart.setOnClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_newbie;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onClick(View v) {
		getActivity().finish();
	}

}
