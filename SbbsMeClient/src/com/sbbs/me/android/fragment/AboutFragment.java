package com.sbbs.me.android.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.FeedbackActivity;
import com.sbbs.me.android.R;

public class AboutFragment extends BaseFragment implements OnClickListener {

	Button btnFeedback;

	public AboutFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_about_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_about;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_about;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		btnFeedback = (Button) innerView.findViewById(R.id.btnFeedback);

	}

	@Override
	public void initEvents() {
		btnFeedback.setOnClickListener(this);

	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_about;
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
		switch (v.getId()) {
		case R.id.btnFeedback:
			startActivity(new Intent(getActivity(), FeedbackActivity.class));
			break;
		}
	}

}
