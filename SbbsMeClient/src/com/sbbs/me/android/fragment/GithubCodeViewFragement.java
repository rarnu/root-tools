package com.sbbs.me.android.fragment;

import org.eclipse.egit.github.core.Blob;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.loader.SbbsCodeViewLoader;
import com.sbbs.me.android.utils.CustomUtils;

public class GithubCodeViewFragement extends BaseFragment implements
		OnLoadCompleteListener<Blob> {

	RelativeLayout layBlob;
	SbbsCodeViewLoader loader;
	Blob blob = null;
	TextView blobLoading;
	int repoType = 0;

	public GithubCodeViewFragement() {
		super();
	}

	public GithubCodeViewFragement(int repoType) {
		super();
		this.repoType = repoType;
		tagText = ResourceUtils
				.getString(repoType == 0 ? R.tag.tag_codeview_fragment_sbbs
						: R.tag.tag_codeview_fragment_android);
		tabTitle = ResourceUtils
				.getString(repoType == 0 ? R.string.project_sbbs_me
						: R.string.project_android);
	}

	@Override
	public int getBarTitle() {
		return R.string.lm_ongithub;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.lm_ongithub;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		layBlob = (RelativeLayout) innerView.findViewById(R.id.layBlob);
		blobLoading = (TextView) innerView.findViewById(R.id.blobLoading);

		String sha = getArguments().getString("sha");
		loader = new SbbsCodeViewLoader(getActivity(), repoType, sha);
	}

	@Override
	public void initEvents() {
		loader.registerListener(0, this);
	}

	@Override
	public void initLogic() {
		blobLoading.setVisibility(View.VISIBLE);
		loader.startLoading();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_codeview;
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
	public void onLoadComplete(Loader<Blob> loader, Blob data) {
		blob = data;
		if (getActivity() != null) {
			if (blob != null) {
				String path = getArguments().getString("path");
				boolean isMarkdown = path.toLowerCase().endsWith(".md")
						|| path.equals("README");
				CustomUtils.addBlock(getActivity(), data, isMarkdown, 0, 0,
						"", layBlob, 210000, 210000, false, null, null);
			}
			blobLoading.setVisibility(View.GONE);
		}
	}

}
