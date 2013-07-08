package com.sbbs.me.android.loader;

import java.util.List;

import org.eclipse.egit.github.core.Repository;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;

public class SbbsGithubLoader extends BaseLoader<Repository> {

	public SbbsGithubLoader(Context context){
		super(context);
	}
	
	@SuppressWarnings("deprecation")
	@Override
	public List<Repository> loadInBackground(){
		List<Repository> list = null;
		try {
			list = SbbsMeAPI.getRepos();
		} catch(Exception e) {
			
		}
		return list;
	}
}
