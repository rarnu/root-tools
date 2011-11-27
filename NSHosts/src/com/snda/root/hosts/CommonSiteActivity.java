package com.snda.root.hosts;

import java.io.File;
import java.io.IOException;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.AdapterView.OnItemClickListener;

import com.snda.root.hosts.utils.FileUtils;

public class CommonSiteActivity extends Activity implements OnItemClickListener {

	private String[] sites = new String[] { "www.google.com",
			"android.clients.google.com", "mail.google.com",
			"search.google.com", "maps.google.com", "talk.google.com",
			"code.google.com", "images.google.com", "news.google.com",
			"reader.google.com", "plus.google.com", "talkgadget.google.com",
			"picadaweb.google.com", "ditu.google.com", "docs.google.com",
			"books.google.com", "sites.google.com", "translate.google.com",
			"id.google.com", "groups.google.com", "www.wikipedia.org",
			"www.facebook.com", "www.twitter.com", "m.twitter.com",
			"www.dropbox.com" };

	ListView lvSites;
	ArrayAdapter<String> adapter = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.common_site);

		lvSites = (ListView) findViewById(R.id.lvSites);
		lvSites.setOnItemClickListener(this);

		showDomainList();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {

		String item = (String) lvSites.getItemAtPosition(position);
		Intent inResult = new Intent();
		inResult.putExtra("DOMAIN", item);
		setResult(RESULT_OK, inResult);
		finish();

	}

	private void showDomainList() {

		File fDomain = new File(GlobalInstance.app_dir
				+ GlobalInstance.domain_filename);
		if (fDomain.exists()) {
			try {
				List<String> domain = FileUtils.readFile(fDomain);
				adapter = new ArrayAdapter<String>(this, R.layout.site_item,
						domain);
			} catch (IOException e) {
			}
		} else {

			adapter = new ArrayAdapter<String>(this, R.layout.site_item, sites);
		}
		lvSites.setAdapter(adapter);
	}

}
