package com.sbbs.me.android.fragment;

import static org.eclipse.egit.github.core.TreeEntry.TYPE_BLOB;
import static org.eclipse.egit.github.core.TreeEntry.TYPE_TREE;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.egit.github.core.TreeEntry;

import android.content.Intent;
import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.CodeViewActivity;
import com.sbbs.me.android.R;
import com.sbbs.me.android.adapter.SbbsMeTreeEntryAdapter;
import com.sbbs.me.android.component.PathView;
import com.sbbs.me.android.component.PathView.PathClickListener;
import com.sbbs.me.android.loader.SbbsCodeTreeLoader;
import com.sbbs.me.android.utils.CustomUtils;

public class GithubCodeTreeFragment extends BaseFragment implements
		OnLoadCompleteListener<List<TreeEntry>>, OnItemClickListener,
		OnClickListener, PathClickListener {

	ListView treeList;
	SbbsCodeTreeLoader loader;
	SbbsMeTreeEntryAdapter adapter;
	TextView tvNodata;
	TextView treeLoading;
	List<TreeEntry> listTreeEntry;
	String sha;
	int repoType = 0;
	HashMap<String, String> parentSha;
	PathView tvPath;

	public GithubCodeTreeFragment() {
		super();
	}

	public GithubCodeTreeFragment(int repoType) {
		super();
		this.repoType = repoType;
		this.parentSha = new HashMap<String, String>();
		tagText = ResourceUtils
				.getString(repoType == 0 ? R.tag.tag_codetree_fragment_sbbs
						: R.tag.tag_codetree_fragment_android);
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
		repoType = getArguments().getInt("repo_type", 0);
		tagText = ResourceUtils
				.getString(repoType == 0 ? R.tag.tag_codetree_fragment_sbbs
						: R.tag.tag_codetree_fragment_android);
		tabTitle = ResourceUtils
				.getString(repoType == 0 ? R.string.project_sbbs_me
						: R.string.project_android);
		CustomUtils.changeFragmentTag(this, tagText);
		getActivity().getActionBar().getTabAt(repoType).setText(tabTitle);

		treeList = (ListView) innerView.findViewById(R.id.treeList);
		treeLoading = (TextView) innerView.findViewById(R.id.treeLoading);
		if (listTreeEntry == null) {
			listTreeEntry = new ArrayList<TreeEntry>();
		}
		parentSha = new HashMap<String, String>();
		adapter = new SbbsMeTreeEntryAdapter(getActivity(), listTreeEntry);
		treeList.setAdapter(adapter);
		loader = new SbbsCodeTreeLoader(getActivity(), repoType, null);
		tvNodata = (TextView) innerView.findViewById(R.id.tvNodata);
		tvPath = (PathView) innerView.findViewById(R.id.tvPath);
	}

	@Override
	public void initEvents() {
		treeList.setOnItemClickListener(this);
		loader.registerListener(0, this);
		tvNodata.setOnClickListener(this);
		tvPath.setPathClickListener(this);
	}

	@Override
	public void initLogic() {
		treeLoading.setVisibility(View.VISIBLE);
		loader.startLoading();
		TreeEntry tr = new TreeEntry();
		tr.setPath("root");
		tr.setSha("");
		tvPath.addPath(tr);
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_codetree;
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
	public void onLoadComplete(Loader<List<TreeEntry>> loader,
			List<TreeEntry> data) {
		listTreeEntry.clear();
		if (data != null) {
			listTreeEntry.addAll(data);
		}
		if (getActivity() != null) {
			tvNodata.setVisibility(listTreeEntry.size() == 0 ? View.VISIBLE
					: View.GONE);
			adapter.setNewList(listTreeEntry);
			endLoad();
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		final TreeEntry item = listTreeEntry.get(position);
		if (!this.parentSha.containsKey(item.getSha())) {
			this.parentSha.put(item.getSha(), this.sha);
		}
		this.sha = item.getSha();
		if (item.getType().equals(TYPE_TREE)) {
			if (item.getPath().equals("..")) {
				tvPath.upLevel();
			} else {
				tvPath.addPath(item);
			}
			treeLoading.setText(R.string.loading);
			loader.setRefresh(false);
			loader.setSha(this.sha);
			loader.setParentSha(this.parentSha);
			startLoad();
		} else if (item.getType().equals(TYPE_BLOB)) {
			startActivity(new Intent(getActivity(), CodeViewActivity.class)
					.putExtra("repoType", repoType)
					.putExtra("sha", item.getSha())
					.putExtra("path", item.getPath()));
		}
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvNodata:
			treeLoading.setText(R.string.loading);
			startLoad();
			break;
		}
	}

	@Override
	public void onPathClick(TreeEntry entry) {
		if (entry.getSha().equals(this.sha)) {
			treeLoading.setText(R.string.Updating);
			loader.setRefresh(true);
		} else {
			loader.setRefresh(false);
			tvPath.gotoPath(entry);
			this.sha = entry.getSha();
			treeLoading.setText(R.string.loading);
			loader.setSha(this.sha);
			loader.setParentSha(this.parentSha);
		}
		startLoad();
	}

	private void startLoad() {
		tvNodata.setEnabled(false);
		treeLoading.setVisibility(View.VISIBLE);
		tvPath.setPathClickListener(null);
		treeList.setOnItemClickListener(null);
		loader.startLoading();
	}

	private void endLoad() {
		tvNodata.setEnabled(true);
		treeLoading.setVisibility(View.GONE);
		tvPath.setPathClickListener(this);
		treeList.setOnItemClickListener(this);
	}
}
