package com.sbbs.me.android.adapter;

import java.util.List;

import org.eclipse.egit.github.core.Repository;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.sbbs.me.android.R;

public class SbbsMeGithubAdapter extends BaseAdapter<Repository> {

	public SbbsMeGithubAdapter(Context context, List<Repository> list) {
		super(context, list);
	}
	
	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View v = convertView;
		if (v == null) {
			v = inflater.inflate(R.layout.item_repo, parent, false);
		}
		SbbsMeRepoHolder holder = (SbbsMeRepoHolder) v.getTag();
		if (holder == null) {
			holder = new SbbsMeRepoHolder();
			holder.repoName = (TextView) v.findViewById(R.id.repoName);
			holder.repoDesc = (TextView) v.findViewById(R.id.repoDesc);
			//holder.repoLanguage = (TextView) v.findViewById(R.id.repoLanguage);
			//holder.repoWatchers = (TextView) v.findViewById(R.id.repoWatchers);
			//holder.repoForks = (TextView) v.findViewById(R.id.repoForks);
			v.setTag(holder);
		}
		Repository item = list.get(position);
		if (item != null) {
			holder.repoName.setText(item.getName());
			holder.repoDesc.setText(item.getDescription());
			//holder.repoLanguage.setText(item.getLanguage());
			//holder.repoWatchers.setText(0);
			//holder.repoForks.setText(item.getForks());
		}
		return v;
	}

	@Override
	public String getValueText(Repository item) {
		return "";
	}

}
