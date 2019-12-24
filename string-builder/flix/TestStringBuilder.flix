/*
 * Copyright 2019 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


/////////////////////////////////////////////////////////////////////////////
// new                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def new01(): StringBuilder = StringBuilder.new()


/////////////////////////////////////////////////////////////////////////////
// appendString                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def appendString01(): Bool = 
    let sb = StringBuilder.new();
    StringBuilder.appendString(sb, "hello");
    StringBuilder.toString(sb) `assertEq!` "hello"

@test
def appendString02(): Bool = 
    let sb = StringBuilder.new();
    StringBuilder.appendString(sb, "hello");
    StringBuilder.appendString(sb, "!");
    StringBuilder.toString(sb) `assertEq!` "hello!"

@test
def appendString03(): Bool = 
    let sb = StringBuilder.new();
    StringBuilder.appendString(sb, "hello");
    StringBuilder.appendString(sb, " ");
    StringBuilder.appendString(sb, "world!");
    StringBuilder.toString(sb) `assertEq!` "hello world!"
