use std::io::File;
use std::io::BufferedReader;

pub fn scope_start(src:&str, point:uint) -> uint {
    let s = src.slice(0,point);
    let mut pt = point;
    let mut levels = 0;
    for c in s.chars_rev() {
        if c == '{' { 
            if levels == 0 {
                break;
            } else {
                levels -= 1;
            }
        }
        if c == '}' {
            levels += 1;
        }
        pt -= 1;
    }
    return pt;
}

fn gen_mask(mut len: uint) -> ~str {
    let mut s = ~"";
    while len != 0 {
        len -=1;
        s.push_str(" ");
    }
    return s;
}

pub fn mask_comments(src:&str) -> ~str {
    let mut result = ~"";
    let mut s:&str = src;
    let mut in_comment = false;
    let mut res = s.find_str("//");
    let mut pos = 0;
    while res.is_some() {
        let end = res.unwrap();
        if in_comment {
            // mask out the comment
            result.push_str(gen_mask(end));
            s = s.slice_from(end);
            res = s.find_str("//");
            in_comment = false;
        } else {
            result.push_str(s.slice_to(end));
            s = s.slice_from(end);
            res = s.find_str("\n");
            in_comment = true;
        }
        pos += end;
    }
    result.push_str(src.slice_from(pos));
    return result;
}

pub fn mask_sub_scopes(src:&str) -> ~str {
    let mut result = ~"";
    let mut levels = 0;
    
    for c in src.chars() {
        if c == '}' {
            levels -= 1;
        }

        if c == '\n' {
            result.push_char(c);
        } else if levels > 0 {
            result.push_str(" ");
        } else {
            result.push_char(c);
        }

        if c == '{' {
            levels += 1;
        } 
    }    
    return result;
}

pub fn coords_to_point(src: &str, mut linenum: uint, col: uint) -> uint {
    let mut point=0;
    for line in src.lines() {
        linenum -= 1;
        if linenum == 0 { break }
        point+=line.len() + 1;  // +1 for the \n
    }
    return point + col;
}

pub fn point_to_coords(src:&str, point:uint) -> (uint, uint) {
    let mut i = 0;
    let mut linestart = 0;
    let mut nlines = 1;  // lines start at 1
    while i != point {
        if src.char_at(i) == '\n' {
            nlines += 1;
            linestart = i+1;
        }
        i+=1;
    }
    return (nlines, point - linestart);
}

pub fn point_to_coords2(path: &Path, point:uint) -> Option<(uint, uint)> {
    let mut lineno = 0;
    let mut file = BufferedReader::new(File::open(path));
    let mut p = 0;
    for line_r in file.lines() {
        let line = line_r.unwrap();
        lineno += 1;
        if point < (p + line.len()) {
            return Some((lineno, point - p));
        }
        p += line.len();
    }
    return None;
}


#[test]
fn coords_to_point_works() {
    let src = "
fn myfn() {
    let a = 3;
    print(a);
}";
    assert!(coords_to_point(src, 3, 5) == 18);
}

#[test]
fn test_scope_start() {
    let src = "
fn myfn() {
    let a = 3;
    print(a);
}
";
    let point = coords_to_point(src, 4, 10);
    let start = scope_start(src,point);
    assert!(start == 12);
}

#[test]
fn test_scope_start_handles_sub_scopes() {
    let src = "
fn myfn() {
    let a = 3;
    {
      let b = 4;
    }
    print(a);
}
";
    let point = coords_to_point(src, 7, 10);
    let start = scope_start(src,point);
    assert!(start == 12);
}



#[test]
fn masks_out_comments() {
    let src = "
this is some code
this is a line // with a comment
some more
";
    let r = mask_comments(src.to_owned());
    assert!(src.len() == r.len());
    // characters at the start are the same
    assert!(src[5] == r[5]);
    // characters in the comments are masked
    let commentoffset = coords_to_point(src,3,23);
    assert!(r.char_at(commentoffset) == ' ');
    assert!(src[commentoffset] != r[commentoffset]);
    // characters afterwards are the same 
    assert!(src[src.len()-3] == r[src.len()-3]);
}


#[test]
fn masks_out_sub_scopes() {
    let src = "
this is some code
{
  this is a sub-scope
  {
    so is this
  }
  and this
}
some more code
";
    let expected = "
this is some code
{
                     
   
              
   
          
}
some more code
";
    let res = mask_sub_scopes(src);
    assert!(expected == res);
}

#[test]
fn test_point_to_coords() {
    let src = "
fn myfn(b:uint) {
   let a = 3;
   if b == 12 {
       let a = 24;
       do_something_with(a);
   }
   do_something_with(a);
}
";
    round_trip_point_and_coords(src, 4, 5);
}

fn round_trip_point_and_coords(src:&str, lineno:uint, charno:uint) {
    let (a,b) = point_to_coords(src, coords_to_point(src, lineno, charno));
     assert_eq!((a,b),(lineno,charno));
}

#[test]
fn generates_the_visible_scope() {
    let src = "
fn myfn(b:uint) {
   let a = 3;
   if b == 12 {
       let a = 24;
       do_something_with(a);
   }
   do_something_with(a);
}
";
    let expected = "
   let a = 3;
   if b == 12 {
                  
                            
   }
   do_something_with(";
    let point = coords_to_point(src,8,21);
    let s = mask_comments(src);
    let n = scope_start(s, point);
    let res = mask_sub_scopes(s.slice(n,point));
    assert!(expected == res);
}

