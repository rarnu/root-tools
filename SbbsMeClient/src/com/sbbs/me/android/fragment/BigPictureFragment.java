package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.ImageView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.EndlessViewPager;
import com.rarnu.utils.DownloadUtils;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.consts.PathDefine;

public class BigPictureFragment extends BaseFragment {

	EndlessViewPager evpPicture;

	public BigPictureFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_big_picture_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.view_image;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.view_image;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		evpPicture = (EndlessViewPager) innerView.findViewById(R.id.evpPicture);
	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {
		List<String> image = getArguments().getStringArrayList("image");
		List<String> url = getArguments().getStringArrayList("url");
		int index = getArguments().getInt("index", 0);
		String currentItem = getArguments().getString("current_item");
		if (image == null || image.size() == 0) {
			getActivity().finish();
		} else {
			initViewPager(image, url, index, currentItem);
		}
	}

	private void initViewPager(List<String> image, List<String> url, int index,
			String currentItem) {
		evpPicture.setEndless(image.size() != 1);
		List<View> views = new ArrayList<View>();
		for (int i = 0; i < image.size(); i++) {
			ImageView iv = new ImageView(getActivity());
			DownloadUtils.downloadFileT(getActivity(), iv, SbbsMeAPI.ROOT_URL
					+ url.get(i), PathDefine.ROOT_PATH, image.get(i), null);
			views.add(iv);
		}
		evpPicture.setData(views);
		setCurrentItem(image, index, currentItem);
	}

	private void setCurrentItem(List<String> image, int index,
			String currentItem) {

		evpPicture.setCurrentItem(Integer.MAX_VALUE / 2
				- (index % image.size()));
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_big_picture;
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

}
