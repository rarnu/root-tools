package com.yugioh.android.fragments;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseDialogFragment;
import com.yugioh.android.R;
import com.yugioh.android.utils.DeviceUtils;

public class AboutFragment extends BaseDialogFragment {

	TextView tvVersion, tvAboutDate;
	
	public AboutFragment(String tagText) {
		super(tagText);
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_about;
	}

	@Override
	protected void initComponents() {
		tvVersion = (TextView) innerView.findViewById(R.id.tvVersion);
		tvAboutDate = (TextView) innerView.findViewById(R.id.tvAboutDate);
	}

	@Override
	protected void initEvents() {

	}

	@Override
	protected void initLogic() {
		tvVersion.setText(DeviceUtils.getAppVersionName(getActivity()));
		String releaseDate = "";
		try {
			ApplicationInfo appInfo = getActivity().getPackageManager().getApplicationInfo(getActivity().getPackageName(),
					PackageManager.GET_META_DATA);
			
			releaseDate = appInfo.metaData.getString("release-date");
			
		} catch (Exception e) {
		}
		tvAboutDate.setText(getString(R.string.about_date_fmt, releaseDate));
	}

}
