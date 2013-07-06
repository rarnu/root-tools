package com.sbbs.me.android.api;

import static java.lang.String.CASE_INSENSITIVE_ORDER;
import static org.eclipse.egit.github.core.TreeEntry.TYPE_BLOB;
import static org.eclipse.egit.github.core.TreeEntry.TYPE_TREE;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.egit.github.core.Tree;
import org.eclipse.egit.github.core.TreeEntry;

@Deprecated
public class SbbsMeCodeTree {
	
	public static class Entry implements Comparable<Entry> {

		public final Folder parent;
		public final TreeEntry entry;
		public final String name;
		
		private Entry() {
			this.parent = null;
			this.entry = null;
			this.name = null;
		}
		
		private Entry(TreeEntry entry, Folder parent) {
			this.entry = entry;
			this.parent = parent;
			this.name = entry.getPath();
		}
		
		@Override
		public int compareTo(Entry another) {
			return CASE_INSENSITIVE_ORDER.compare(name, another.name);
		}
		
	}
	
	public static class Folder extends Entry {
		
		public final Map<String, Folder> folders = 
				new TreeMap<String, Folder>(CASE_INSENSITIVE_ORDER);
		
		public final Map<String, Entry> files = 
				new TreeMap<String, Entry>(CASE_INSENSITIVE_ORDER);
		
		private Folder() {
			super();
		}
		
		private Folder(TreeEntry entry, Folder parent) {
			super(entry, parent);
		}
		
		private void addFile(TreeEntry entry, String[] pathSegments, int index) {
            if (index == pathSegments.length - 1) {
                Entry file = new Entry(entry, this);
                files.put(file.name, file);
            } else {
                Folder folder = folders.get(pathSegments[index]);
                if (folder != null)
                    folder.addFile(entry, pathSegments, index + 1);
            }
        }

        private void addFolder(TreeEntry entry, String[] pathSegments, int index) {
            if (index == pathSegments.length - 1) {
                Folder folder = new Folder(entry, this);
                folders.put(folder.name, folder);
            } else {
                Folder folder = folders.get(pathSegments[index]);
                if (folder != null)
                    folder.addFolder(entry, pathSegments, index + 1);
            }
        }

        private void add(final TreeEntry entry) {
            String type = entry.getType();
            String path = entry.getPath();
            if ( path == null || path.length() == 0 )
                return;

            if (TYPE_BLOB.equals(type)) {
                String[] segments = path.split("/");
                if (segments.length > 1) {
                    Folder folder = folders.get(segments[0]);
                    if (folder != null)
                        folder.addFile(entry, segments, 1);
                } else if (segments.length == 1) {
                    Entry file = new Entry(entry, this);
                    files.put(file.name, file);
                }
            } else if (TYPE_TREE.equals(type)) {
                String[] segments = path.split("/");
                if (segments.length > 1) {
                    Folder folder = folders.get(segments[0]);
                    if (folder != null)
                        folder.addFolder(entry, segments, 1);
                } else if (segments.length == 1) {
                    Folder folder = new Folder(entry, this);
                    folders.put(folder.name, folder);
                }
            }
        } 
	}
	
    public final Tree tree;
    public final Folder root;
    
    public SbbsMeCodeTree(final Tree tree) {
		this.tree = tree;
		root = new Folder();
		List<TreeEntry> entries = tree.getTree();
		if (entries != null && !entries.isEmpty()) {
			for (TreeEntry entry : entries) {
				root.add(entry);
			}
		}
	}
}
