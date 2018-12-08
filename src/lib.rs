use std::{fmt, str};
use std::time::SystemTime;
use std::collections::BTreeMap;

pub enum Error {
    InvalidPath,
    InvalidTarget,
    NotFound,
    AlreadyExists,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::InvalidPath   => write!(f, "invalid path"),
            Error::InvalidTarget => write!(f, "invalid target"),
            Error::NotFound      => write!(f, "not found"),
            Error::AlreadyExists => write!(f, "already exists"),
        }
    }
}

impl From<Error> for std::io::Error {
    fn from(e: Error) -> std::io::Error {
        match e {
            Error::InvalidPath   => std::io::Error::new(
                std::io::ErrorKind::InvalidInput, "invalid path"),
            Error::InvalidTarget => std::io::Error::new(
                std::io::ErrorKind::InvalidInput, "invalid target"),
            Error::NotFound      => std::io::Error::new(
                std::io::ErrorKind::NotFound, "not found"),
            Error::AlreadyExists => std::io::Error::new(
                std::io::ErrorKind::AlreadyExists, "already exists"),
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

fn invalid_path<T>() -> Result<T> {
    Err(Error::InvalidPath)
}

fn invalid_target<T>() -> Result<T> {
    Err(Error::InvalidTarget)
}

fn not_found<T>() -> Result<T> {
    Err(Error::NotFound)
}

fn file_exists<T>() -> Result<T> {
    Err(Error::AlreadyExists)
}

pub type Index = usize;

pub struct Dir {
    nlink: usize,
    pub mtime: SystemTime,
    pub uid: u32,
    pub gid: u32,
    pub mode: u16,
    entries: BTreeMap<Box<[u8]>, Index>,
    parent: Index,
}

impl Dir {
    pub fn nlink(&self) -> usize {
        self.nlink
    }

    pub fn parent(&self) -> Index {
        self.parent
    }

    pub fn entries(&self) -> impl Iterator<Item=(&[u8], Index)>
            + ExactSizeIterator
            + DoubleEndedIterator {
        self.entries.iter().map(|(k,&v)| (k.as_ref(), v))
    }
}

pub struct File<T> {
    nlink: usize,
    pub mtime: SystemTime,
    pub uid: u32,
    pub gid: u32,
    pub mode: u16,
    pub data: T,
}

impl<T> File<T> {
    pub fn nlink(&self) -> usize {
        self.nlink
    }
}

pub struct Symlink {
    nlink: usize,
    pub mtime: SystemTime,
    pub uid: u32,
    pub gid: u32,
    pub mode: u16,
    tgt: Box<[u8]>,
}

impl Symlink {
    pub fn nlink(&self) -> usize {
        self.nlink
    }

    pub fn target(&self) -> &[u8] {
        self.tgt.as_ref()
    }

    fn validate_target(tgt: &[u8]) -> Result<()> {
        if tgt.is_empty() || tgt.contains(&b'\0') {
            return invalid_target();
        }
        Ok(())
    }

    pub fn update(&mut self, tgt: &[u8]) -> Result<&mut Self> {
        Self::validate_target(tgt)?;
        self.tgt = tgt.into();
        Ok(self)
    }
}

pub struct BlockDev {
    nlink: usize,
    pub mtime: SystemTime,
    pub uid: u32,
    pub gid: u32,
    pub mode: u16,
    pub rdev: u32,
}

impl BlockDev {
    pub fn nlink(&self) -> usize {
        self.nlink
    }
}

pub struct CharDev {
    nlink: usize,
    pub mtime: SystemTime,
    pub uid: u32,
    pub gid: u32,
    pub mode: u16,
    pub rdev: u32,
}

impl CharDev {
    pub fn nlink(&self) -> usize {
        self.nlink
    }
}

pub struct Fifo {
    nlink: usize,
    pub mtime: SystemTime,
    pub uid: u32,
    pub gid: u32,
    pub mode: u16,
}

impl Fifo {
    pub fn nlink(&self) -> usize {
        self.nlink
    }
}

pub struct Socket {
    nlink: usize,
    pub mtime: SystemTime,
    pub uid: u32,
    pub gid: u32,
    pub mode: u16,
}

impl Socket {
    pub fn nlink(&self) -> usize {
        self.nlink
    }
}

pub enum Node<T> {
    Dir(Dir),
    File(File<T>),
    Symlink(Symlink),
    BlockDev(BlockDev),
    CharDev(CharDev),
    Fifo(Fifo),
    Socket(Socket),
}

impl<T> Node<T> {
    pub fn nlink(&self) -> usize {
        match *self {
            Node::Dir(ref d)      => d.nlink,
            Node::File(ref f)     => f.nlink,
            Node::Symlink(ref l)  => l.nlink,
            Node::CharDev(ref c)  => c.nlink,
            Node::BlockDev(ref b) => b.nlink,
            Node::Fifo(ref p)     => p.nlink,
            Node::Socket(ref s)   => s.nlink,
        }
    }

    pub fn mtime(&self) -> SystemTime {
        match *self {
            Node::Dir(ref d)      => d.mtime,
            Node::File(ref f)     => f.mtime,
            Node::Symlink(ref l)  => l.mtime,
            Node::CharDev(ref c)  => c.mtime,
            Node::BlockDev(ref b) => b.mtime,
            Node::Fifo(ref p)     => p.mtime,
            Node::Socket(ref s)   => s.mtime,
        }
    }

    pub fn uid(&self) -> u32 {
        match *self {
            Node::Dir(ref d)      => d.uid,
            Node::File(ref f)     => f.uid,
            Node::Symlink(ref l)  => l.uid,
            Node::CharDev(ref c)  => c.uid,
            Node::BlockDev(ref b) => b.uid,
            Node::Fifo(ref p)     => p.uid,
            Node::Socket(ref s)   => s.uid,
        }
    }

    pub fn gid(&self) -> u32 {
        match *self {
            Node::Dir(ref d)      => d.gid,
            Node::File(ref f)     => f.gid,
            Node::Symlink(ref l)  => l.gid,
            Node::CharDev(ref c)  => c.gid,
            Node::BlockDev(ref b) => b.gid,
            Node::Fifo(ref p)     => p.gid,
            Node::Socket(ref s)   => s.gid,
        }
    }

    pub fn mode(&self) -> u16 {
        match *self {
            Node::Dir(ref d)      => d.mode,
            Node::File(ref f)     => f.mode,
            Node::Symlink(ref l)  => l.mode,
            Node::CharDev(ref c)  => c.mode,
            Node::BlockDev(ref b) => b.mode,
            Node::Fifo(ref p)     => p.mode,
            Node::Socket(ref s)   => s.mode,
        }
    }

    pub fn touch(&mut self, mtime: SystemTime) -> &mut Self {
        match *self {
            Node::Dir(ref mut d)      => d.mtime = mtime,
            Node::File(ref mut f)     => f.mtime = mtime,
            Node::Symlink(ref mut l)  => l.mtime = mtime,
            Node::CharDev(ref mut c)  => c.mtime = mtime,
            Node::BlockDev(ref mut b) => b.mtime = mtime,
            Node::Fifo(ref mut p)     => p.mtime = mtime,
            Node::Socket(ref mut s)   => s.mtime = mtime,
        }
        self
    }

    pub fn chown(&mut self, uid: u32) -> &mut Self {
        match *self {
            Node::Dir(ref mut d)      => d.uid = uid,
            Node::File(ref mut f)     => f.uid = uid,
            Node::Symlink(ref mut l)  => l.uid = uid,
            Node::CharDev(ref mut c)  => c.uid = uid,
            Node::BlockDev(ref mut b) => b.uid = uid,
            Node::Fifo(ref mut p)     => p.uid = uid,
            Node::Socket(ref mut s)   => s.uid = uid,
        }
        self
    }

    pub fn chgrp(&mut self, gid: u32) -> &mut Self {
        match *self {
            Node::Dir(ref mut d)      => d.gid = gid,
            Node::File(ref mut f)     => f.gid = gid,
            Node::Symlink(ref mut l)  => l.gid = gid,
            Node::CharDev(ref mut c)  => c.gid = gid,
            Node::BlockDev(ref mut b) => b.gid = gid,
            Node::Fifo(ref mut p)     => p.gid = gid,
            Node::Socket(ref mut s)   => s.gid = gid,
        }
        self
    }

    pub fn chmod(&mut self, mode: u16) -> &mut Self {
        match *self {
            Node::Dir(ref mut d)      => d.mode = mode,
            Node::File(ref mut f)     => f.mode = mode,
            Node::Symlink(ref mut l)  => l.mode = mode,
            Node::CharDev(ref mut c)  => c.mode = mode,
            Node::BlockDev(ref mut b) => b.mode = mode,
            Node::Fifo(ref mut p)     => p.mode = mode,
            Node::Socket(ref mut s)   => s.mode = mode,
        }
        self
    }

    pub fn is_dir(&self) -> bool {
        match *self {
            Node::Dir(_) => true,
            _            => false,
        }
    }

    pub fn is_file(&self) -> bool {
        match *self {
            Node::File(_) => true,
            _             => false,
        }
    }

    pub fn is_symlink(&self) -> bool {
        match *self {
            Node::Symlink(_) => true,
            _                => false,
        }
    }
}

enum Entry<T> {
    Node(Node<T>),
    Unused(Index),
}

impl<T> Entry<T> {
    fn nlink(&self) -> usize {
        match *self {
            Entry::Node(Node::Dir(ref d))      => d.nlink,
            Entry::Node(Node::File(ref f))     => f.nlink,
            Entry::Node(Node::Symlink(ref l))  => l.nlink,
            Entry::Node(Node::CharDev(ref c))  => c.nlink,
            Entry::Node(Node::BlockDev(ref b)) => b.nlink,
            Entry::Node(Node::Fifo(ref f))     => f.nlink,
            Entry::Node(Node::Socket(ref s))   => s.nlink,
            Entry::Unused(_)                   => 1,
        }
    }
}

pub struct VirtFS<T> {
    entry: Vec<Entry<T>>,
    nodes: usize,
    free: Index,
}

impl<T> Default for VirtFS<T> {
    fn default() -> Self {
        VirtFS::new()
    }
}

impl<T> VirtFS<T> {
    pub fn new_at(mtime: SystemTime) -> Self {
        Self {
            entry: vec![Entry::Node(Node::Dir(Dir {
                nlink: 2,
                mtime,
                uid: 0,
                gid: 0,
                mode: 0o755,
                entries: BTreeMap::new(),
                parent: 0,
            }))],
            nodes: 1,
            free: 0,
        }
    }

    pub fn new() -> Self {
        Self::new_at(SystemTime::now())
    }

    pub fn nodes(&self) -> usize {
        self.nodes
    }

    pub fn max_idx(&self) -> Index {
        self.entry.len()
    }

    pub fn mtime(&self) -> SystemTime {
        if let Some(&Entry::Node(Node::Dir(Dir { mtime, .. }))) = self.entry.get(0) {
            mtime
        } else {
            unreachable!();
        }
    }

    pub fn touch(&mut self, st: SystemTime) {
        if let Some(&mut Entry::Node(Node::Dir(ref mut d))) = self.entry.get_mut(0) {
            d.mtime = st;
        } else {
            unreachable!();
        }
    }

    fn resolve<'a>(&self, orig: &'a [u8]) -> Result<(Index, &'a [u8])> {
        let mut stack = Vec::new();
        let mut path = orig;
        let mut end = 0;
        let mut idx = 0;
        let mut dir = match self.entry[idx] {
            Entry::Node(Node::Dir(ref d)) => d,
            _ => unreachable!(),
        };

        loop {
            while end < path.len() && path[end] == b'/' {
                end += 1;
            }
            let start = end;
            while end < path.len() && path[end] != b'/' {
                if path[end] == b'\0' {
                    return invalid_path();
                }
                end += 1;
            }
            /* return if this is the last component */
            if end == path.len() && stack.is_empty() {
                return Ok((idx, &orig[start..end]));
            }
            /* follow this link */
            let part = &path[start..end];
            if part.is_empty() {
                return invalid_path();
            } else if part == b"." {
                /* do nothing */
            } else if part == b".." {
                idx = dir.parent;
                dir = match self.entry[idx] {
                    Entry::Node(Node::Dir(ref d)) => d,
                    _ => unreachable!(),
                };
            } else if let Some(&child) = dir.entries.get(part) {
                match self.entry[child] {
                    Entry::Node(Node::Dir(ref d)) => {
                        idx = child;
                        dir = d;
                    }
                    Entry::Node(Node::Symlink(Symlink { ref tgt, .. })) => {
                        stack.push((path, end));
                        path = tgt;
                        end = 0;
                    }
                    _ => return not_found(),
                };
            } else {
                return not_found();
            }
            if end == path.len() {
                let (p, e) = stack.pop().unwrap();
                path = p;
                end = e;
            }
        }
    }

    pub fn lookup(&self, path: &[u8]) -> Result<Index> {
        let (idx, name) = self.resolve(path)?;
        if name.is_empty() {
            return Ok(idx);
        }
        if let Entry::Node(Node::Dir(ref d)) = self.entry[idx] {
            return match d.entries.get(name) {
                Some(&idx) => Ok(idx),
                None       => not_found(),
            }
        }
        unreachable!()
    }

    pub fn node(&self, idx: Index) -> Option<&Node<T>> {
        match self.entry.get(idx) {
            Some(Entry::Node(ref n)) => Some(n),
            _ => None,
        }
    }

    pub fn node_mut(&mut self, idx: Index) -> Option<&mut Node<T>> {
        match self.entry.get_mut(idx) {
            Some(Entry::Node(ref mut n)) => Some(n),
            _ => None,
        }
    }

    pub fn get(&self, path: &[u8]) -> Option<&Node<T>> {
        if let Ok(n) = self.lookup(path) {
            return self.node(n);
        }
        None
    }

    pub fn get_mut(&mut self, path: &[u8]) -> Option<&mut Node<T>> {
        if let Ok(n) = self.lookup(path) {
            return self.node_mut(n);
        }
        None
    }

    fn link_at(&mut self, at: Index, name: &[u8], idx: Index) -> Result<()> {
        if let Entry::Node(Node::Dir(ref mut dir)) = self.entry[at] {
            if dir.entries.contains_key(name) {
                return file_exists();
            }
            dir.entries.insert(name.into(), idx);
        } else {
            return not_found();
        }
        Ok(())
    }

    fn newlink_at(&mut self, at: Index, name: &[u8]) -> Result<Index> {
        if name.is_empty() {
            return invalid_path();
        }
        if self.free == 0 {
            self.free = self.entry.len();
            self.entry.push(Entry::Unused(0))
        }
        let idx = self.free;
        self.link_at(at, name, idx)?;
        if let Entry::Unused(next) = self.entry[idx] {
            self.free = next;
        } else {
            panic!("free points to used entry")
        }
        Ok(idx)
    }

    pub fn mkdir_at(&mut self, at: Index, name: &[u8]) -> Result<Index> {
        let idx = self.newlink_at(at, name)?;
        self.entry[idx] = Entry::Node(Node::Dir(Dir {
            nlink: 2,
            mtime: self.mtime(),
            uid: 0,
            gid: 0,
            mode: 0o755,
            entries: BTreeMap::new(),
            parent: at,
        }));
        self.nodes += 1;
        if let Entry::Node(Node::Dir(ref mut p)) = self.entry[at] {
            p.nlink += 1;
        }
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn mkdir(&mut self, path: &[u8]) -> Result<&mut Node<T>> {
        let (parent, name) = self.resolve(path)?;
        let idx = self.mkdir_at(parent, name)?;
        if let Entry::Node(ref mut n) = self.entry[idx] {
            return Ok(n);
        }
        unreachable!()
    }

    pub fn newfile_at<A: Into<T>>(&mut self, at: Index, name: &[u8], data: A) -> Result<Index> {
        let idx = self.newlink_at(at, name)?;
        self.entry[idx] = Entry::Node(Node::File(File {
            nlink: 1,
            mtime: self.mtime(),
            uid: 0,
            gid: 0,
            mode: 0o644,
            data: data.into(),
        }));
        self.nodes += 1;
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn newfile<A: Into<T>>(&mut self, path: &[u8], data: A) -> Result<&mut Node<T>> {
        let (parent, name) = self.resolve(path)?;
        let idx = self.newfile_at(parent, name, data)?;
        if let Entry::Node(ref mut n) = self.entry[idx] {
            return Ok(n);
        }
        unreachable!()
    }

    pub fn symlink_at(&mut self, at: Index, name: &[u8], tgt: &[u8]) -> Result<Index> {
        Symlink::validate_target(tgt)?;
        let idx = self.newlink_at(at, name)?;
        self.entry[idx] = Entry::Node(Node::Symlink(Symlink {
            nlink: 1,
            mtime: self.mtime(),
            uid: 0,
            gid: 0,
            mode: 0o777,
            tgt: tgt.into(),
        }));
        self.nodes += 1;
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn symlink(&mut self, path: &[u8], tgt: &[u8]) -> Result<&mut Node<T>> {
        let (parent, name) = self.resolve(path)?;
        let idx = self.symlink_at(parent, name, tgt)?;
        if let Entry::Node(ref mut n) = self.entry[idx] {
            return Ok(n);
        }
        unreachable!()
    }

    pub fn blockdev_at(&mut self, at: Index, name: &[u8], rdev: u32) -> Result<Index> {
        let idx = self.newlink_at(at, name)?;
        self.entry[idx] = Entry::Node(Node::BlockDev(BlockDev {
            nlink: 1,
            mtime: self.mtime(),
            uid: 0,
            gid: 0,
            mode: 0o644,
            rdev,
        }));
        self.nodes += 1;
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn blockdev(&mut self, path: &[u8], rdev: u32) -> Result<&mut Node<T>> {
        let (parent, name) = self.resolve(path)?;
        let idx = self.blockdev_at(parent, name, rdev)?;
        if let Entry::Node(ref mut n) = self.entry[idx] {
            return Ok(n);
        }
        unreachable!()
    }

    pub fn chardev_at(&mut self, at: Index, name: &[u8], rdev: u32) -> Result<Index> {
        let idx = self.newlink_at(at, name)?;
        self.entry[idx] = Entry::Node(Node::CharDev(CharDev {
            nlink: 1,
            mtime: self.mtime(),
            uid: 0,
            gid: 0,
            mode: 0o644,
            rdev,
        }));
        self.nodes += 1;
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn chardev(&mut self, path: &[u8], rdev: u32) -> Result<&mut Node<T>> {
        let (parent, name) = self.resolve(path)?;
        let idx = self.chardev_at(parent, name, rdev)?;
        if let Entry::Node(ref mut n) = self.entry[idx] {
            return Ok(n);
        }
        unreachable!()
    }

    pub fn fifo_at(&mut self, at: Index, name: &[u8]) -> Result<Index> {
        let idx = self.newlink_at(at, name)?;
        self.entry[idx] = Entry::Node(Node::Fifo(Fifo {
            nlink: 1,
            mtime: self.mtime(),
            uid: 0,
            gid: 0,
            mode: 0o644,
        }));
        self.nodes += 1;
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn fifo(&mut self, path: &[u8]) -> Result<&mut Node<T>> {
        let (parent, name) = self.resolve(path)?;
        let idx = self.fifo_at(parent, name)?;
        if let Entry::Node(ref mut n) = self.entry[idx] {
            return Ok(n);
        }
        unreachable!()
    }

    pub fn socket_at(&mut self, at: Index, name: &[u8]) -> Result<Index> {
        let idx = self.newlink_at(at, name)?;
        self.entry[idx] = Entry::Node(Node::Socket(Socket {
            nlink: 1,
            mtime: self.mtime(),
            uid: 0,
            gid: 0,
            mode: 0o644,
        }));
        self.nodes += 1;
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn socket(&mut self, path: &[u8]) -> Result<&mut Node<T>> {
        let (parent, name) = self.resolve(path)?;
        let idx = self.socket_at(parent, name)?;
        if let Entry::Node(ref mut n) = self.entry[idx] {
            return Ok(n);
        }
        unreachable!()
    }

    pub fn hardlink_at(&mut self, at: Index, name: &[u8], idx: Index) -> Result<()> {
        if let Entry::Node(Node::Dir(_)) = self.entry[idx] {
            return invalid_target();
        }
        if name.is_empty() {
            return invalid_path();
        }
        self.link_at(at, name, idx)?;
        match self.entry[idx] {
            Entry::Node(Node::File(ref mut f))     => f.nlink += 1,
            Entry::Node(Node::Symlink(ref mut l))  => l.nlink += 1,
            Entry::Node(Node::CharDev(ref mut c))  => c.nlink += 1,
            Entry::Node(Node::BlockDev(ref mut b)) => b.nlink += 1,
            Entry::Node(Node::Fifo(ref mut p))     => p.nlink += 1,
            Entry::Node(Node::Socket(ref mut s))   => s.nlink += 1,
            Entry::Node(Node::Dir(_))              => unreachable!(),
            Entry::Unused(_)                       => unreachable!(),
        }
        debug_assert!(self.validate());
        Ok(())
    }

    pub fn hardlink(&mut self, path: &[u8], tgt: &[u8]) -> Result<()> {
        let (parent, name) = self.resolve(path)?;
        let idx = self.lookup(tgt)?;
        self.hardlink_at(parent, name, idx)
    }

    pub fn unlink_at(&mut self, at: Index, name: &[u8]) -> Result<()> {
        if name.is_empty() {
            return invalid_path();
        }
        let idx = if let Entry::Node(Node::Dir(ref mut p)) = self.entry[at] {
            if let Some(idx) = p.entries.remove(name) {
                idx
            } else {
                return not_found()
            }
        } else {
            return not_found()
        };
        if let Entry::Node(Node::Dir(_)) = self.entry[idx] {
            if let Entry::Node(Node::Dir(ref mut p)) = self.entry[at] {
                p.nlink -= 1;
            } else {
                panic!("parent points to non-directory");
            }
        }
        let mut stack = vec![idx];
        while let Some(idx) = stack.pop() {
            match self.entry[idx] {
                Entry::Node(Node::Dir(ref d)) => {
                    for &v in d.entries.values() {
                        stack.push(v);
                    }
                }
                Entry::Node(Node::File(ref mut f)) => {
                    f.nlink -= 1;
                    if f.nlink > 0 {
                        continue;
                    }
                }
                Entry::Node(Node::Symlink(ref mut l)) => {
                    l.nlink -= 1;
                    if l.nlink > 0 {
                        continue;
                    }
                }
                Entry::Node(Node::BlockDev(ref mut b)) => {
                    b.nlink -= 1;
                    if b.nlink > 0 {
                        continue;
                    }
                }
                Entry::Node(Node::CharDev(ref mut c)) => {
                    c.nlink -= 1;
                    if c.nlink > 0 {
                        continue;
                    }
                }
                Entry::Node(Node::Fifo(ref mut p)) => {
                    p.nlink -= 1;
                    if p.nlink > 0 {
                        continue;
                    }
                }
                Entry::Node(Node::Socket(ref mut s)) => {
                    s.nlink -= 1;
                    if s.nlink > 0 {
                        continue;
                    }
                }
                Entry::Unused(_) => panic!("directory entry points to unused inode"),
            }
            self.entry[idx] = Entry::Unused(self.free);
            self.nodes -= 1;
            self.free = idx;
        }
        debug_assert!(self.validate());
        Ok(())
    }

    pub fn unlink(&mut self, path: &[u8]) -> Result<()> {
        let (parent, name) = self.resolve(path)?;
        self.unlink_at(parent, name)
    }

    pub fn validate(&self) -> bool {
        let len = self.entry.len();
        let mut nlinks: Box<[usize]> = vec![0; len].into();
        { /* walk free list */
            let mut idx = self.free;
            while idx > 0 {
                if idx >= len {
                    return false;
                }
                if let Entry::Unused(next) = self.entry[idx] {
                    nlinks[idx] += 1;
                    idx = next;
                } else {
                    return false;
                }
            }
        }
        /* count node links and check parent references */
        let mut nodes = 0;
        nlinks[0] += 1; /* count imaginary link to root dir */
        for (idx, e) in self.entry.iter().enumerate() {
            match *e {
                Entry::Node(Node::Dir(ref d)) => {
                    nodes += 1;
                    nlinks[idx] += 1; /* "." link */
                    for &child in d.entries.values() {
                        if child >= len {
                            return false;
                        }
                        match self.entry[child] {
                            Entry::Node(Node::Dir(ref c)) => {
                                if c.parent != idx {
                                    return false;
                                }
                                nlinks[idx] += 1; /* ".." link */
                                nlinks[child] += 1;
                            }
                            Entry::Unused(_) => {
                                return false;
                            }
                            _ => {
                                nlinks[child] += 1;
                            }
                        }
                    }
                }
                Entry::Node(_)   => nodes += 1,
                Entry::Unused(_) => {}
            }
        }
        if nodes != self.nodes {
            return false;
        }
        /* check link counts match */
        for (i, &nlink) in nlinks.iter().enumerate() {
            if nlink == 0 || nlink != self.entry[i].nlink() {
                return false;
            }
        }
        true
    }
}

fn to_str(buf: &[u8]) -> &str {
    str::from_utf8(buf).unwrap_or("<non-utf8>")
}

impl<T> fmt::Display for VirtFS<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut path = String::from("/");
        let mut stack = vec![("", 0, path.len())];

        while let Some((name, idx, len)) = stack.pop() {
            path.truncate(len);
            path.push_str(name);
            match self.entry[idx] {
                Entry::Node(Node::Dir(ref d)) => {
                    writeln!(f, "d {:3} {:2} {:4} {:4} {:4o} {} {}", idx,
                             d.nlink, d.uid, d.gid, d.mode, &path, d.parent)?;
                    if !path.ends_with('/') {
                        path.push('/');
                    }
                    for (n, i) in d.entries.iter().rev() {
                        stack.push((to_str(n), *i, path.len()));
                    }
                }
                Entry::Node(Node::File(ref r)) => {
                    writeln!(f, "f {:3} {:2} {:4} {:4} {:4o} {}", idx,
                             r.nlink, r.uid, r.gid, r.mode, &path)?;
                }
                Entry::Node(Node::Symlink(ref s)) => {
                    writeln!(f, "l {:3} {:2} {:4} {:4} {:4o} {} -> {}", idx,
                             s.nlink, s.uid, s.gid, s.mode, &path, to_str(&s.tgt))?;
                }
                Entry::Node(Node::BlockDev(ref b)) => {
                    writeln!(f, "b {:3} {:2} {:4} {:4} {:4o} {} {}:{}", idx,
                             b.nlink, b.uid, b.gid, b.mode, &path, b.rdev >> 8, b.rdev & 0xff)?;
                }
                Entry::Node(Node::CharDev(ref c)) => {
                    writeln!(f, "c {:3} {:2} {:4} {:4} {:4o} {} {}:{}", idx,
                             c.nlink, c.uid, c.gid, c.mode, &path, c.rdev >> 8, c.rdev & 0xff)?;
                }
                Entry::Node(Node::Fifo(ref p)) => {
                    writeln!(f, "p {:3} {:2} {:4} {:4} {:4o} {}", idx,
                             p.nlink, p.uid, p.gid, p.mode, &path)?;
                }
                Entry::Node(Node::Socket(ref s)) => {
                    writeln!(f, "s {:3} {:2} {:4} {:4} {:4o} {}", idx,
                             s.nlink, s.uid, s.gid, s.mode, &path)?;
                }
                Entry::Unused(_) => panic!("directory entry points to unused inode"),
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{Node, VirtFS};

    #[test]
    fn new() {
        let fs = VirtFS::<&str>::new();

        assert_eq!(fs.nodes(), 1);
        assert!(fs.lookup(b"").is_ok());
        assert!(fs.lookup(b"/").is_ok());
        assert!(fs.lookup(b"/.//").is_ok());
        assert!(fs.lookup(b"not found").is_err());
        assert!(fs.lookup(b"invalid\0name").is_err());
        assert!(fs.validate());
    }

    #[test]
    fn mkdir() {
        let mut fs = VirtFS::<&str>::new();

        assert!(fs.mkdir(b"").is_err());
        assert!(fs.mkdir(b"invalid\0name").is_err());
        assert!(fs.mkdir(b"boot").is_ok());
        assert!(fs.mkdir(b"/.//boot").is_err());
        assert!(fs.mkdir(b"usr/lib").is_err());
        assert!(fs.mkdir(b"/.///usr").is_ok());
        assert!(fs.mkdir(b"usr").is_err());
        assert!(fs.mkdir(b"usr/../usr/../usr/lib").is_ok());
        assert!(fs.validate());
    }

    #[test]
    fn newfile() {
        let mut fs = VirtFS::<&str>::new();

        assert!(fs.newfile(b"file1", "abc\n").is_ok());
        assert!(fs.newfile(b"/./file1", "abc\n").is_err());
        assert!(fs.newfile(b"/a/file2", "xyz\n").is_err());
        assert!(fs.mkdir(b"a").is_ok());
        assert!(fs.newfile(b"/a/.//../a/file2", "xyz\n").is_ok());
        assert!(fs.validate());

        let f = match fs.get(b"a/file2") {
            Some(Node::File(ref f)) => f,
            _ => panic!(),
        };
        assert_eq!(f.data, "xyz\n");
    }

    #[test]
    fn symlink() {
        let mut fs = VirtFS::<&str>::new();

        assert!(fs.symlink(b"", b"tgt").is_err());
        assert!(fs.symlink(b"invalid\0name", b"tgt").is_err());
        assert!(fs.symlink(b"link1", b"invalid\0target").is_err());
        assert!(fs.symlink(b"link1", b"").is_err());
        assert!(fs.symlink(b"link1", b"tgt").is_ok());
        assert!(fs.symlink(b"link1", b"tgt").is_err());
        assert!(fs.symlink(b"a/link", b"..").is_err());
        assert!(fs.mkdir(b"a").is_ok());
        assert!(fs.symlink(b"a/link", b"..").is_ok());
        assert!(fs.symlink(b"a/link", b"..").is_err());
        assert!(fs.mkdir(b"a/link/b").is_ok());
        assert!(fs.get(b"/b").unwrap().is_dir());
        assert!(fs.validate());
    }

    #[test]
    fn blockdev() {
        let mut fs = VirtFS::<&str>::new();
        let sda = 8u32 << 8 | 0u32;

        assert!(fs.blockdev(b"", sda).is_err());
        assert!(fs.blockdev(b"invalid\0name", sda).is_err());
        assert!(fs.blockdev(b"dev/sda", sda).is_err());
        assert!(fs.mkdir(b"dev").is_ok());
        assert!(fs.blockdev(b"dev/sda", sda).is_ok());
        assert!(fs.blockdev(b"dev/sda", sda).is_err());
        assert!(fs.blockdev(b"/.//dev/..//./dev/sda", sda).is_err());
        assert!(fs.validate());
    }

    #[test]
    fn chardev() {
        let mut fs = VirtFS::<&str>::new();
        let null = 1u32 << 8 | 3u32;

        assert!(fs.chardev(b"", null).is_err());
        assert!(fs.chardev(b"invalid\0name", null).is_err());
        assert!(fs.chardev(b"dev/null", null).is_err());
        assert!(fs.mkdir(b"dev").is_ok());
        assert!(fs.chardev(b"dev/null", null).is_ok());
        assert!(fs.chardev(b"dev/null", null).is_err());
        assert!(fs.chardev(b"/.//dev/..//./dev/null", null).is_err());
        assert!(fs.validate());
    }

    #[test]
    fn fifo() {
        let mut fs = VirtFS::<&str>::new();

        assert!(fs.fifo(b"").is_err());
        assert!(fs.fifo(b"invalid\0name").is_err());
        assert!(fs.fifo(b"dev/init").is_err());
        assert!(fs.mkdir(b"dev").is_ok());
        assert!(fs.fifo(b"dev/init").is_ok());
        assert!(fs.fifo(b"dev/init").is_err());
        assert!(fs.fifo(b"/.//dev/..//./dev/init").is_err());
        assert!(fs.validate());
    }

    #[test]
    fn socket() {
        let mut fs = VirtFS::<&str>::new();

        assert!(fs.socket(b"").is_err());
        assert!(fs.socket(b"invalid\0name").is_err());
        assert!(fs.socket(b"dev/log").is_err());
        assert!(fs.mkdir(b"dev").is_ok());
        assert!(fs.socket(b"dev/log").is_ok());
        assert!(fs.socket(b"dev/log").is_err());
        assert!(fs.socket(b"/.//dev/..//./dev/log").is_err());
        assert!(fs.validate());
    }

    #[test]
    fn hardlink() {
        let mut fs = VirtFS::<&str>::new();
        let sda = 8u32 << 8 | 0u32;
        let null = 1u32 << 8 | 3u32;

        assert!(fs.mkdir(b"a").is_ok());
        assert!(fs.newfile(b"a/file", "xyz\n").is_ok());
        assert!(fs.symlink(b"a/symlink", b"..").is_ok());
        assert!(fs.blockdev(b"a/blockdev", sda).is_ok());
        assert!(fs.chardev(b"a/chardev", null).is_ok());
        assert!(fs.fifo(b"a/fifo").is_ok());
        assert!(fs.socket(b"a/socket").is_ok());
        assert!(fs.mkdir(b"b").is_ok());
        assert!(fs.hardlink(b"b/test", b"a/doesn't exist").is_err());
        assert!(fs.hardlink(b"b/dir", b"a").is_err());
        assert!(fs.hardlink(b"b/file", b"a/file").is_ok());
        assert!(fs.hardlink(b"b/symlink", b"a/symlink").is_ok());
        assert!(fs.hardlink(b"b/blockdev", b"a/blockdev").is_ok());
        assert!(fs.hardlink(b"b/chardev", b"a/chardev").is_ok());
        assert!(fs.hardlink(b"b/fifo", b"a/fifo").is_ok());
        assert!(fs.hardlink(b"b/socket", b"a/socket").is_ok());
        assert_eq!(fs.get(b"a/file").unwrap().nlink(), 2);
        assert_eq!(fs.get(b"a/symlink").unwrap().nlink(), 2);
        fs.get_mut(b"b/file").unwrap().chown(1000).chgrp(100);
        assert_eq!(fs.get(b"a/file").unwrap().uid(), 1000);
        assert_eq!(fs.get(b"a/file").unwrap().gid(), 100);
        assert!(fs.validate());

        let f = match fs.get(b"b/file") {
            Some(Node::File(ref f)) => f,
            _ => panic!(),
        };
        assert_eq!(f.data, "xyz\n");

        let s = match fs.get(b"b/symlink") {
            Some(Node::Symlink(ref s)) => s,
            _ => panic!(),
        };
        assert_eq!(s.target(), b"..");

        let b = match fs.get(b"b/blockdev") {
            Some(Node::BlockDev(ref b)) => b,
            _ => panic!(),
        };
        assert_eq!(b.rdev, sda);

        let c = match fs.get(b"b/chardev") {
            Some(Node::CharDev(ref c)) => c,
            _ => panic!(),
        };
        assert_eq!(c.rdev, null);

        fs.get_mut(b"a/fifo").unwrap().chown(6).chgrp(7);
        let p = match fs.get(b"b/fifo") {
            Some(Node::Fifo(ref p)) => p,
            _ => panic!(),
        };
        assert_eq!(p.uid, 6);
        assert_eq!(p.gid, 7);

        fs.get_mut(b"a/socket").unwrap().chown(8).chgrp(9);
        let s = match fs.get(b"b/socket") {
            Some(Node::Socket(ref s)) => s,
            _ => panic!(),
        };
        assert_eq!(s.uid, 8);
        assert_eq!(s.gid, 9);
    }

    #[test]
    fn unlink() {
        let mut fs = VirtFS::<&str>::new();
        let sda = 8u32 << 8 | 0u32;
        let null = 1u32 << 8 | 3u32;

        assert!(fs.unlink(b"").is_err());
        assert!(fs.unlink(b"/").is_err());
        assert!(fs.unlink(b"//././//").is_err());
        assert!(fs.mkdir(b"a").is_ok());
        assert!(fs.mkdir(b"a/b").is_ok());
        assert!(fs.mkdir(b"a/b/c").is_ok());
        assert!(fs.newfile(b"a/b/c/file", "xyz\n").is_ok());
        assert!(fs.symlink(b"a/b/c/symlink", b"..").is_ok());
        assert!(fs.blockdev(b"a/b/c/blockdev", sda).is_ok());
        assert!(fs.chardev(b"a/b/c/chardev", null).is_ok());
        assert!(fs.newfile(b"a/b/anotherfile", "abc\n").is_ok());
        assert!(fs.newfile(b"a/b/c/stays", "survivor").is_ok());
        assert!(fs.hardlink(b"stays", b"a/b/c/stays").is_ok());
        assert!(fs.unlink(b"a/b").is_ok());
        assert_eq!(fs.nodes(), 3);
    }
}
